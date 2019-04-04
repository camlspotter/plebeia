(** Implementation of space-efficient binary Patricia trees in OCaml.
    The implementation is geared for used in Tezos, though it is rather
    generic. A stop-and-copy GC is provided. This implementation aims
    to maximize correctness and cares second about efficiency. Extracting
    and efficient C program from F* should be explored. *)

open Utils
open Error
open Stdint

type error = string
type value = Value.t
type segment = Path.segment
type hash = Hash.hash56

module Index = Uint32
(* XXX Optimization for 64bit arch is possible *)

type index = Index.t

type indexed_implies_hashed =
  | Indexed_and_Hashed
  | Not_Indexed_Any
  (* Type used to prove that all indexed nodes have been hashed. *)

type hashed_is_transitive =
  | Hashed of Hash.hash56
  | Not_Hashed
  (* Type used to prove that if a node is hashed then so are its children.
     The type also provides the hash as a witness.*)

type internal
type not_internal

type indexing_rule =
  | Indexed of Index.t
  | Left_Not_Indexed (* Right may not be indexed either *)
  | Right_Not_Indexed (* Left may not be indexed either *)
  | Not_Indexed
  (* This rule expresses the following invariant : if a node is indexed, then
     its children are necessarily indexed. Less trivially, if an internal node is not
     indexed then at least one of its children is not yet indexed. The reason
     is that we never construct new nodes that just point to only existing nodes. 
     This property guarantees that when we write internal nodes on
     disk, at least one of the child can be written adjacent to its parent. *)

type extender_witness =
  | Maybe_Extender
  | Not_Extender  
  | Is_Extender   

type hashed_witness =
  | Hashed of Hash.hash56
  | Not_Hashed

type indexed_witness =
  | Indexed of Index.t
  | Not_Indexed

module Context = struct
  type t = {
    array : Bigstring.t ;
    (* mmaped array where the nodes are written and indexed. *)
    mutable length : Index.t ;
    (* Current length of the node table. *)
    leaf_table  : KVS.t ;
    (* Hash table  mapping leaf hashes to their values. *)
    store_in_leaf_table : bool ;
    (* If [false], all the values are stored in the tree *)
    roots_table : (Hash.hash56, Index.t) Hashtbl.t ;
    (* Hash table mapping root hashes to indices in the array. *)
    fd : Unix.file_descr ; 
    (* File descriptor to the mapped file *)
  }

  let make ?pos ?(shared=false) ?(length=(-1)) ?(use_kvs=false) fn =
    let fd = Unix.openfile fn [O_RDWR] 0o644 in
    let array =
      (* length = -1 means that the size of [fn] determines the size of
         [array]. This is almost certainly NOT what we want. Rather the array
         should be made x% bigger than the file (say x ~ 25%). When the array
         is close to being full, the file should be closed and reopened with
         a bigger length.
      *) (* FIXME *)
      let open Bigarray in
      array1_of_genarray @@ Unix.map_file fd ?pos
        char c_layout shared [| length |] in
    { array ;
      length = Uint32.zero ;
      leaf_table = KVS.make () ;
      store_in_leaf_table = use_kvs;
      roots_table = Hashtbl.create 1 ;
      fd = fd ;
    }

  let new_index c =
    (* XXX check of size *)
    let i = Uint32.succ c.length in
    c.length <- i;
    i
    
  let new_indices c n =
    (* XXX check of size *)
    assert (n > 0);
    let i = Uint32.succ c.length in
    c.length <- Index.(c.length + of_int n);
    i
    
  let free { fd ; _ } = Unix.close fd
end

module PrivateNode : sig 

  (* Constructors of view, trail, and cursor are private, 
     to prevent invalid values formed 
  *)
     
  type node =
    | Disk of Index.t * extender_witness
    (* Represents a node stored on the disk at a given index, the node hasn't
       been loaded yet. Although it's considered hash for simplicity's sake,
       reading the hash requires a disk access and is expensive. *)

    | View of view
    (* A view node is the in-memory structure manipulated by programs in order
       to compute edits to the context. New view nodes can be commited to disk
       once the computations are done. *)

  and view = private
    | Internal of node * node
                 * indexing_rule
                 * hashed_is_transitive
                 * indexed_implies_hashed

    (* An internal node , left and right children and an internal path segment
       to represent part of the path followed by the key in the tree. *)

    | Bud of node option
            * indexing_rule
            * hashed_is_transitive
            * indexed_implies_hashed
    (* Buds represent the end of a segment and the beginning of a new tree. They
       are used whenever there is a natural hierarchical separation in the key
       or, in general, when one wants to be able to grab sub-trees. For instance
       the big_map storage of a contract in Tezos would start from a bud. *)

    | Leaf of Value.t
            * indexing_rule
            * hashed_is_transitive
            * indexed_implies_hashed
    (* Leaf of a tree, the end of a path, contains or points to a value.
       The current implementation is a bit hackish and leaves are written
       on *two* cells, not one. This is important to keep in mind when
       committing the tree to disk.
    *)

    | Extender of Path.segment
                  * node
                  * indexing_rule
                  * hashed_is_transitive
                  * indexed_implies_hashed
    (* Extender node, contains a path to the next node. Represents implicitely
       a collection of internal nodes where one child is Null. *)

  (* A trail represents the content of the memory stack when recursively exploring a tree.
     Constructing these trails from closure would be easier, but it would make it harder
     to port the code to C. The type parameters of the trail keep track of the type of each
     element on the "stack" using a product type. *)

  val indexed : node -> bool
  val index : node -> Index.t option
  val hashed : node -> bool
  val hash_of_view : view -> Hash.hash56 option

  val _Internal : node * node
                 * indexing_rule
                 * hashed_is_transitive
                 * indexed_implies_hashed -> view
  val _Bud : node option
          * indexing_rule
          * hashed_is_transitive
          * indexed_implies_hashed -> view

  val _Leaf : Value.t
          * indexing_rule
          * hashed_is_transitive
          * indexed_implies_hashed -> view

  val _Extender : Path.segment
                 * node
                 * indexing_rule
                 * hashed_is_transitive
                 * indexed_implies_hashed -> view

  type modified_rule =
    | Modified_Left
    | Modified_Right
    | Unmodified of
        indexing_rule *
        hashed_is_transitive

  type trail = private
    | Top
    | Left of (* we took the left branch of an internal node *)
        trail
        * node
        * modified_rule
        * indexed_implies_hashed

    | Right of (* we took the right branch of an internal node *)
        node
        * trail
        * modified_rule
        * indexed_implies_hashed

    | Budded of
        trail
        * modified_rule
        * indexed_implies_hashed

    | Extended of
        trail
        * Path.segment
        * modified_rule
        * indexed_implies_hashed

  val _Top : trail
  val _Left : trail
      * node
      * modified_rule
      * indexed_implies_hashed -> trail
  val _Right : 
      node
      * trail
      * modified_rule
      * indexed_implies_hashed -> trail
  val _Budded :
      trail
      * modified_rule
      * indexed_implies_hashed -> trail
  val _Extended :
      trail
      * Path.segment
      * modified_rule
      * indexed_implies_hashed -> trail

  val load_node_ref : (Context.t -> Index.t -> extender_witness -> view) ref
  val load_node : Context.t -> Index.t -> extender_witness -> view

  val view : Context.t -> node -> view

  type cursor = private
      Cursor of trail
                * node
                * Context.t
  (* The cursor, also known as a zipper combines the information contained in a
     trail and a subtree to represent an edit point within a tree. This is a
     functional data structure that represents the program point in a function
     that modifies a tree. We use an existential type that keeps the .mli sane
     and enforces the most important: that the hole tags match between the trail
     and the Node *)

  val _Cursor : (trail * node * Context.t) -> cursor

end = struct

  type node =
    | Disk of Index.t * extender_witness
    (* Represents a node stored on the disk at a given index, the node hasn't
       been loaded yet. Although it's considered hash for simplicity's sake,
       reading the hash requires a disk access and is expensive. *)

    | View of view
    (* A view node is the in-memory structure manipulated by programs in order
       to compute edits to the context. New view nodes can be commited to disk
       once the computations are done. *)

  and view =
    | Internal of node * node
                 * indexing_rule
                 * hashed_is_transitive
                 * indexed_implies_hashed

    (* An internal node , left and right children and an internal path segment
       to represent part of the path followed by the key in the tree. *)

    | Bud of node option
            * indexing_rule
            * hashed_is_transitive
            * indexed_implies_hashed
    (* Buds represent the end of a segment and the beginning of a new tree. They
       are used whenever there is a natural hierarchical separation in the key
       or, in general, when one wants to be able to grab sub-trees. For instance
       the big_map storage of a contract in Tezos would start from a bud. *)

    | Leaf of Value.t
            * indexing_rule
            * hashed_is_transitive
            * indexed_implies_hashed
    (* Leaf of a tree, the end of a path, contains or points to a value.
       The current implementation is a bit hackish and leaves are written
       on *two* cells, not one. This is important to keep in mind when
       committing the tree to disk.
    *)

    | Extender of Path.segment
                  * node
                  * indexing_rule
                  * hashed_is_transitive
                  * indexed_implies_hashed
    (* Extender node, contains a path to the next node. Represents implicitely
       a collection of internal nodes where one child is Null. *)

  (* A trail represents the content of the memory stack when recursively exploring a tree.
     Constructing these trails from closure would be easier, but it would make it harder
     to port the code to C. The type parameters of the trail keep track of the type of each
     element on the "stack" using a product type. *)

  let view_shape_invariant : view -> (unit, error) result = function
    | Bud (None, _, _, _) -> Ok ()
    | Bud (Some (Disk _), _, _, _) -> Error "Bud: cannot have Disk" (* or, we must load the Disk and check *)
    | Bud (Some (View (Bud _)), _, _, _) -> Error "Bud: cannot have Bud"
    | Bud (Some (View (Leaf _)), _, _, _) -> Error "Bud: cannot have Leaf"
    | Bud (Some (View (Internal _)), _, _, _) -> Ok ()
    | Bud (Some (View (Extender _)), _, _, _) -> Ok ()
    | Extender (seg, _, _, _, _) when Path.is_empty seg -> Error "Extender: cannot have empty segment"
    | Extender (_, Disk (_, Not_Extender), _, _, _) -> Ok ()
    | Extender (_, Disk (_, Is_Extender), _, _, _) -> Error "Extender: cannot have Disk with Is_Extender"
    | Extender (_, Disk (_, Maybe_Extender), _, _, _) -> Error "Extender: cannot have Disk with Maybe_Extender"
    | Extender (_, View (Extender _), _, _, _) -> Error "Extender: cannot have Extender"
    | Extender (_, View _, _, _, _) -> Ok ()
    | Leaf _ -> Ok ()
    | Internal _ -> Ok ()

  let indexed = function
    | Disk _ -> true
    | View (Bud (_, Indexed _, _, _)) -> true
    | View (Leaf (_, Indexed _, _, _)) -> true
    | View (Internal (_, _, Indexed _, _, _)) -> true
    | View (Extender (_, _, Indexed _, _, _)) -> true
    | _ -> false

  let index = function
    | Disk (i,_) -> Some i
    | View (Bud (_, Indexed i, _, _)) -> Some i
    | View (Leaf (_, Indexed i, _, _)) -> Some i
    | View (Internal (_, _, Indexed i, _, _)) -> Some i
    | View (Extender (_, _, Indexed i, _, _)) -> Some i
    | _ -> None

  let view_indexing_rule_invariant : view -> (unit, error) result = function
    | Bud (None, Indexed _, _, _) -> Ok ()
    | Bud (Some n, Indexed _, _, _) when indexed n -> Ok ()
    | Bud (_, (Left_Not_Indexed | Right_Not_Indexed), _, _) -> Error "Bud: invalid indexing_rule"
    | Bud (_, Not_Indexed, _, _) -> Ok ()
    | Leaf (_, Indexed _, _, _) -> Ok ()
    | Leaf (_, Not_Indexed, _, _) -> Ok ()
    | Leaf (_, (Left_Not_Indexed | Right_Not_Indexed), _, _) -> Error "Leaf: invalid indexing_rule"
    | Internal (l, r, Indexed i, _, _) ->
        begin match index l, index r with
          | None, _ -> Error "Internal: invalid Indexed"
          | _, None -> Error "Internal: invalid Indeced"
          | Some li, Some ri -> 
              if Index.(i - li = one || i - ri = one) then Ok ()
              else Error "Internal: invalid indices"
        end
    | Internal (l, _r, Left_Not_Indexed, _, _) when not @@ indexed l -> Ok ()
    | Internal (_l, r, Right_Not_Indexed, _, _) when not @@ indexed r -> Ok ()
    | Internal (_l, _r, Not_Indexed, _, _) -> Error "Internal: invalid indexing_rule"
    | Extender (_, n, Indexed _, _, _) when indexed n -> Ok ()
    | Extender (_, _, Not_Indexed, _, _) -> Ok ()
    | Extender (_, _, (Left_Not_Indexed | Right_Not_Indexed), _, _) -> Error "Bud: invalid indexing_rule"
    | Bud (_, Indexed _, _, _)  
    | Extender (_, _, Indexed _, _, _)  -> Error "Invalid Indexed"
    | Internal (_, _, Left_Not_Indexed, _, _) -> Error "Internal: invalid Left_Not_Indexed"
    | Internal (_, _, Right_Not_Indexed, _, _) -> Error "Internal: invalid Right_Not_Indexed"

  let hashed = function
    | Disk _ -> true
    | View (Bud (_, _, Hashed _, _)) -> true
    | View (Bud (_, _, Not_Hashed, _)) -> false
    | View (Leaf (_, _, Hashed _, _)) -> true
    | View (Leaf (_, _, Not_Hashed, _)) -> false
    | View (Internal (_, _, _, Hashed _, _)) -> true
    | View (Internal (_, _, _, Not_Hashed, _)) -> false
    | View (Extender (_, _, _, Hashed _, _)) -> true
    | View (Extender (_, _, _, Not_Hashed, _)) -> false

  let hash_of_view = function
    | (Bud (_, _, Hashed h, _)) -> Some h
    | (Bud (_, _, Not_Hashed, _)) -> None
    | (Leaf (_, _, Hashed h, _)) -> Some h
    | (Leaf (_, _, Not_Hashed, _)) -> None
    | (Internal (_, _, _, Hashed h, _)) -> Some h
    | (Internal (_, _, _, Not_Hashed, _)) -> None
    | (Extender (_, _, _, Hashed h, _)) -> Some h
    | (Extender (_, _, _, Not_Hashed, _)) -> None

  let view_hashed_is_transitive_invariant : view -> (unit, error) result = function
    | Leaf _ -> Ok ()
    | Bud (None, _, _, _) -> Ok ()
    | Bud (_, _, Not_Hashed, _) -> Ok ()
    | Bud (Some n, _, Hashed _, _) when hashed n -> Ok ()
    | Internal (l, r, _, Hashed _, _) when hashed l && hashed r -> Ok ()
    | Internal (_, _, _, Not_Hashed, _) -> Ok ()
    | Extender (_, n, _, Hashed _, _) when hashed n -> Ok ()
    | Extender (_, _, _, Not_Hashed, _) -> Ok ()
    | _ -> Error "Invalid Hashed"

  let view_index_and_hash_invariant : view -> (unit, error) result = function
    | Bud (_, Indexed _, Not_Hashed, _)
    | Leaf (_, Indexed _, Not_Hashed, _)
    | Internal (_, _, Indexed _, Not_Hashed, _)
    | Extender (_, _, Indexed _, Not_Hashed, _) -> Error "View: Indexed with Not_Hashed"
    | _ -> Ok ()

  let view_invariant : view -> (unit, error) result = fun v ->
    view_shape_invariant v >>= fun () ->
    view_indexing_rule_invariant v >>= fun () ->
    view_hashed_is_transitive_invariant v >>= fun () ->
    view_index_and_hash_invariant v

  let check_view v = 
    match view_invariant v with
    | Ok _ -> v
    | Error s -> failwith s

  let _Internal (n1, n2, ir, hit, iih) =
    check_view @@ Internal (n1, n2, ir, hit, iih)

  let _Bud (nopt, ir, hit, iih) =
    check_view @@ Bud (nopt, ir, hit, iih)

  let _Leaf (v, ir, hit, iih) =
    check_view @@ Leaf (v, ir, hit, iih)

  let _Extender (p, n, ir, hit, iih) =
    check_view @@ Extender (p, n, ir, hit, iih)

  type modified_rule =
    | Modified_Left
    | Modified_Right
    | Unmodified of
        indexing_rule *
        hashed_is_transitive

  (* 'iXXX : indexing
     'hXXX : hashed
     'eXXX : extender
  *)
  type trail =
    | Top
    | Left of (* we took the left branch of an internal node *)
        trail
        * node
        * modified_rule
        * indexed_implies_hashed

    | Right of (* we took the right branch of an internal node *)
        node
        * trail
        * modified_rule
        * indexed_implies_hashed

    | Budded of
        trail
        * modified_rule
        * indexed_implies_hashed

    | Extended of
        trail
        * Path.segment
        * modified_rule
        * indexed_implies_hashed
    (* not the use of the "extender" and "not extender" type to enforce
       that two extenders cannot follow each other *)

  let trail_shape_invariant = function
    | Extended (Extended _, _, _, _) -> Error "Extended: cannot have Extended"
    | Extended (_, seg, _, _) when Path.is_empty seg -> Error "Extended: invalid empty segment"
    | _ -> Ok ()

  let trail_modified_rule_invariant = function
    | Top -> Ok ()
    | Left (_, n, Unmodified (ir, hit), _) -> 
        begin match ir with
          | Left_Not_Indexed -> Ok ()
          | Right_Not_Indexed when not @@ indexed n -> Ok ()
          | Right_Not_Indexed -> Error "Left: invalid Right_Not_Indexed"
          | Not_Indexed -> Error "Left: invalid Not_Indexed"
          | Indexed _ when indexed n -> Ok ()
          | Indexed _ -> Error "Left: invalid Indexed"
        end >>= fun () ->
        begin match hit with
          | Hashed _ when hashed n -> Ok ()
          | Hashed _ -> Error "Left: invalid Hashed"
          | Not_Hashed -> Ok ()
        end
    | Left (_, _, Modified_Left, _) -> Ok ()
    | Left (_, _, Modified_Right, _) -> Error "Left: invalid Modified_Right"
    | Right (n, _, Unmodified (ir, hit) , _) ->
        begin match ir with
          | Right_Not_Indexed -> Ok ()
          | Left_Not_Indexed when not @@ indexed n -> Ok ()
          | Left_Not_Indexed -> Error "Left: invalid Right_Not_Indexed"
          | Not_Indexed -> Error "Right: invalid Not_Indexed"
          | Indexed _ when indexed n -> Ok ()
          | Indexed _ -> Error "Right: invalid Indexed"
        end >>= fun () ->
        begin match hit with
          | Hashed _ when hashed n -> Ok ()
          | Hashed _ -> Error "Right: invalid Hashed"
          | Not_Hashed -> Ok ()
        end
    | Right (_, _, Modified_Right, _) -> Ok ()
    | Right (_, _, Modified_Left, _) -> Error "Right: invalid Modified_Left"
    | Budded (_, Unmodified (ir, _hit), _) ->
        begin match ir with
          | Indexed _ | Not_Indexed -> Ok ()
          | Right_Not_Indexed | Left_Not_Indexed -> Error "Budded: invalid indexing_rule"
        end
    | Budded (_, Modified_Left, _) -> Ok () 
    | Budded (_, Modified_Right, _) -> Error "Budded: invalid Modified_Right"
    | Extended (_, _, Unmodified (ir, _hit), _) ->
        begin match ir with
          | Indexed _ | Not_Indexed -> Ok ()
          | Right_Not_Indexed | Left_Not_Indexed -> Error "Extended: invalid indexing_rule"
        end
    | Extended (_, _, Modified_Left, _) -> Ok () 
    | Extended (_, _, Modified_Right, _) -> Error "Budded: invalid Modified_Right"

  let trail_index_and_hash_invariant = function
    | Top -> Ok ()
    | Left (_, _, Unmodified (Indexed _, Not_Hashed), _)
    | Right (_, _, Unmodified (Indexed _, Not_Hashed) , _)
    | Budded (_, Unmodified (Indexed _, Not_Hashed), _)
    | Extended (_, _, Unmodified (Indexed _, Not_Hashed), _) -> Error "Trail: Indexed with Not_Hashed"
    | _ -> Ok ()

  let trail_invariant t = 
    trail_shape_invariant t >>= fun () ->
    trail_modified_rule_invariant t >>= fun () ->
    trail_index_and_hash_invariant t

  let check_trail t = 
    match trail_invariant t with
    | Ok _ -> t
    | Error s -> failwith s

  let _Top = Top
  let _Left (t, n, mr, iih) = 
    check_trail @@ Left (t, n, mr, iih)
  let _Right (n, t, mr, iih) =
    check_trail @@ Right (n, t, mr, iih)
  let _Budded (t, mr, iih) =
    check_trail @@ Budded (t, mr, iih)
  let _Extended (t, s, mr, iih) =
    check_trail @@ Extended (t, s, mr, iih)

  let load_node_ref = ref (fun _ _ _ -> assert false)

  let load_node context index ewit = !load_node_ref context index ewit

  type cursor =
      Cursor of trail
                * node
                * Context.t
  (* The cursor, also known as a zipper combines the information contained in a
     trail and a subtree to represent an edit point within a tree. This is a
     functional data structure that represents the program point in a function
     that modifies a tree. We use an existential type that keeps the .mli sane
     and enforces the most important: that the hole tags match between the trail
     and the Node *)

  let view c = function
    | Disk (i, wit) -> load_node c i wit
    | View v -> v

  let cursor_invariant (Cursor (trail, n, c)) =
    match trail with
    | Top -> 
        begin match view c n with
          | Bud _ -> Ok ()
          | _ -> Error "Cursor: Top has no Bud"
        end
    | Left (_, _, Unmodified (ir, hit), _) -> 
        begin match ir with
          | Left_Not_Indexed when not @@ indexed n -> Ok ()
          | Left_Not_Indexed -> Error "Cursor: invalid Left_Not_Indexed"
          | Right_Not_Indexed -> Ok ()
          | Not_Indexed -> Error "Cursor: invalid Not_Indexed"
          | Indexed _ when indexed n -> Ok ()
          | Indexed _ -> Error "Cursor: invalid Indexed"
        end >>= fun () ->
        begin match hit with
          | Hashed _ when hashed n -> Ok ()
          | Hashed _ -> Error "Cursor: invalid Hashed"
          | Not_Hashed -> Ok ()
        end
    | Left (_, _, Modified_Left, _) -> Ok ()
    | Left (_, _, Modified_Right, _) -> Error "Left: invalid Modified_Right"
    | Right (_, _, Unmodified (ir, hit) , _) ->
        begin match ir with
          | Left_Not_Indexed -> Ok ()
          | Right_Not_Indexed when not @@ indexed n -> Ok ()
          | Right_Not_Indexed -> Error "Cursor: invalid Right_Not_Indexed"
          | Not_Indexed -> Error "Cursor: invalid Not_Indexed"
          | Indexed _ when indexed n -> Ok ()
          | Indexed _ -> Error "Cursor: invalid Indexed"
        end >>= fun () ->
        begin match hit with
          | Hashed _ when hashed n -> Ok ()
          | Hashed _ -> Error "Cursor: invalid Hashed"
          | Not_Hashed -> Ok ()
        end
    | Right (_, _, Modified_Right, _) -> Ok ()
    | Right (_, _, Modified_Left, _) -> Error "Right: invalid Modified_Left"
    | Budded (_, Unmodified (ir, _hit), _) ->
        begin match ir with
          | Indexed _ when indexed n -> Ok ()
          | Indexed _ -> Error "Budded: invalid Indexed"
          | Not_Indexed -> Ok ()
          | Right_Not_Indexed | Left_Not_Indexed -> Error "Budded: invalid indexing_rule"
        end
    | Budded (_, Modified_Left, _) -> Ok () 
    | Budded (_, Modified_Right, _) -> Error "Budded: invalid Modified_Right"
    | Extended (_, _, Unmodified (ir, hit), _) ->
        begin match ir with
          | Indexed _ when indexed n -> Ok ()
          | Indexed _ -> Error "Extended: invalid Indexed"
          | Not_Indexed -> Ok ()
          | Right_Not_Indexed | Left_Not_Indexed -> Error "Extended: invalid indexing_rule"
        end >>= fun () ->
        begin match hit with
          | Hashed _ when hashed n -> Ok ()
          | Hashed _ -> Error "Extended: invalid Hashed"
          | Not_Hashed -> Ok ()
        end
    | Extended (_, _, Modified_Left, _) -> Ok () 
    | Extended (_, _, Modified_Right, _) -> Error "Budded: invalid Modified_Right"

  let check_cursor c = 
    match cursor_invariant c with
    | Ok _ -> c
    | Error s -> failwith s

  let _Cursor (t, n, c) = 
    check_cursor @@ Cursor (t, n, c)
end

include PrivateNode

let path_of_trail trail =
  let rec aux (xs, xss) = function
    | Top -> xs :: xss
    | Budded (tr, _, _) -> aux ([], xs::xss) tr
    | Left (tr, _, _, _) -> aux (Path.Left :: xs, xss) tr
    | Right (_, tr, _, _) -> aux (Path.Right :: xs, xss) tr
    | Extended (tr, seg, _, _) -> aux ((seg :> Path.side list) @ xs, xss) tr
  in
  aux ([], []) trail

module NodeHash : sig
  open Hash 
  val of_empty_bud : hash56
  val of_bud : hash56 option -> hash56
  val of_leaf : value -> hash56
  val of_internal : hash56 -> hash56 -> hash56
  val of_extender : Path.segment -> hash56 -> hash56
  val of_extender' : segment_code:string -> hash56 -> hash56 
end = struct

  open Hash
 
  let of_leaf v =
    extend_to_hash56 @@ hash_list [ "\000"; Value.to_string v]
  
  (* XXX correct? *)
  let of_empty_bud = hash56_of_string @@ String.make 56 '\000'
  
  (* the hash of a bud node is the hash of its child *)
  let of_bud = function
    | None -> of_empty_bud
    | Some h -> h

  (*
     |<-     H(0x01 || l || h)    ->|
     |                          |0|0|0...........................01|
  *)
  let of_internal l r =
    extend_to_hash56 
    @@ reset_last_2bits 
    @@ hash_list [ "\001"; to_string l; to_string r ]
  
  (*
     |<-                       H_child                           ->|
     | The first 224bits of H_child |0......01|<- segment bits ->|1|
  *) 
  let of_extender seg h =
    hash56_of_string (String.sub (to_string h) 0 28 ^ Segment_encoding.encode seg)
  
  let of_extender' ~segment_code (h : hash56) =
    hash56_of_string (String.sub (to_string h) 0 28 ^ segment_code)
end

module Storage : sig
  exception LoadFailure of error
  val parse_cell : Context.t -> Index.t -> view
  val write : Context.t -> view -> unit

  val write_small_leaf : Context.t -> Value.t -> unit
  val write_large_leaf_to_kvs : Context.t -> Hash.hash28 -> Value.t -> unit
  val write_large_leaf_to_plebeia : Context.t -> Value.t -> unit

  module Chunk : sig
    (* XXX move to tests *)
    val test_write_read : Random.State.t -> Context.t -> unit
  end
end = struct
  (** node storage *)

  exception LoadFailure of error

  module C = struct
    (* Cstruct uses *int32* for uint32. *)
    include Cstruct.LE (* Intel friendly *)
    let get_uint32 buf x = Uint32.of_int32 @@ get_uint32 buf x
    let set_uint32 buf x v = set_uint32 buf x @@ Uint32.to_int32 v
  end

  (* Cstruct.blit_from_string, but make sure all the string contents are written *)
  let write_string s buf off len =
    let slen = String.length s in
    if slen <> len then begin Format.eprintf "write_string: %d <> %d@." slen len; assert false end;
    Cstruct.blit_from_string s 0 buf off len

  (* See Layout.md for the format *)

  let make_buf context i =
    let i = Index.to_int i in (* XXX May overflow in 32bits arch! *)
    Cstruct.of_bigarray ~off:(i*32) ~len:32 context.Context.array

  (* get the last 32 bits *)
  let get_index buf : Index.t = Index.of_uint32 @@ C.get_uint32 buf 28

  let set_index buf i = C.set_uint32 buf 28 @@ Index.to_uint32 i

  (* get the first 224 bits *)
  let get_hash buf = Hash.hash28_of_string @@ Cstruct.copy buf 0 28

  module Chunk = struct

    let ncells size = (size + 8 + 31) / 32
      
    let get_footer_fields context last_index =
      let buf = make_buf context last_index in
      let cdr = C.get_uint32 buf 28 in
      let size = C.get_uint16 buf 26 in
      (cdr, size)
  
    let chunk_contents context ~first_index nbytes =
      Cstruct.of_bigarray ~off:(Uint32.to_int first_index * 32) ~len:nbytes context.Context.array
  
    let get_chunk context last_index =
      let cdr, size = get_footer_fields context last_index in
      (* Format.eprintf "Loading from %d size=%d@." (Index.to_int last_index) size; *)
      let ncells = ncells size in
      let first_index = Uint32.(last_index - of_int ncells + one) in
      (chunk_contents context ~first_index size, size, cdr)
  
    let get_chunks context last_index =
      let rec aux (bufs, size) last_index =
        let buf, bytes, cdr = get_chunk context last_index in
        let bufs = buf :: bufs in
        let size = size + bytes in (* overflow in 32bit? *)
        if cdr = Index.zero then (bufs, size)
        else aux (bufs, size) cdr
      in
      aux ([], 0) last_index
        
    let write_to_chunk context cdr s off len =
      assert (String.length s >= off + len);
      let ncells = ncells len in
      let cdr_pos = ncells * 32 - 4 in
      let size_pos = cdr_pos - 2 in

      let i = Context.new_indices context ncells in
      let last_index = Index.(i + of_int ncells - one) in
      let chunk = chunk_contents context ~first_index:i (32 * ncells) in
      
      Cstruct.blit_from_string s off chunk 0 len;
      (* Format.eprintf "Blit to %d %S@." (Index.to_int last_index) (String.sub s off len); *)
      C.set_uint16 chunk size_pos len;
      C.set_uint32 chunk cdr_pos cdr;
      last_index
        
    let write_to_chunks context max_cells_per_chunk s =
      let max_bytes_per_chunk = 32 * max_cells_per_chunk - 6 in
      let rec f off remain cdr  =
        let len = if remain > max_bytes_per_chunk then max_bytes_per_chunk else remain in
        let cdr' = write_to_chunk context cdr s off len in
        let off' = off + len in
        let remain' = remain - len in
        if remain' > 0 then f off' remain' cdr'
        else cdr'
      in
      f 0 (String.length s) Index.zero
        
    let string_of_cstructs bufs = 
      String.concat "" @@ List.map Cstruct.to_string bufs

    let test_write_read st context =
      let max_cells_per_chunk = Random.State.int st 246 + 10 in
      let size = Random.State.int st (max_cells_per_chunk * 32) + 32 in
      let s = String.init size @@ fun i -> Char.chr (Char.code 'A' + i mod 20) in
      let i = write_to_chunks context max_cells_per_chunk s in
      let s' = string_of_cstructs @@ fst @@ get_chunks context i in
(*
      if s <> s' then begin
        Format.eprintf "%S %S@." s s';
        assert (s = s')
      end;
      prerr_endline "done"
*)
      assert (s = s')
  end

  let rec parse_cell context i =
    let buf = make_buf context i in
    let tag = get_index buf in
    let tag_int32 = Uint32.to_int32 tag in (* easier to match *)
    match tag_int32 with
    | -34l -> (* bud *)
        begin match Cstruct.get_char buf 0 with
          | '\255' -> 
              _Bud (None, Indexed i, Hashed NodeHash.of_empty_bud, Indexed_and_Hashed)
          | _ ->  
              (* We must load the child for the hash *)
              let i' = C.get_uint32 buf 24 in
              let v = parse_cell context i' in
              begin match hash_of_view v with
              | None -> assert false
              | Some h -> _Bud (Some (View v), Indexed i, Hashed h, Indexed_and_Hashed)
              end
        end

    | -33l -> (* leaf whose value is in the KVS *)
        let h = get_hash buf in
        begin match KVS.get_opt context.Context.leaf_table h with
          | None -> raise (LoadFailure (Printf.sprintf "Hash %s is not found in KVS" @@ to_hex @@ Hash.to_string h))
          | Some v -> _Leaf (v, Indexed i, Hashed (Hash.extend_to_hash56 h), Indexed_and_Hashed)
        end

    | x when -32l <= x && x <= -1l -> (* leaf whose value is in the previous cell *)
        let l = 33 + Int32.to_int x in (* 1 to 32 *)
        let h = get_hash buf in
        let buf = make_buf context (Index.pred i) in
        let v = Value.of_string @@ Cstruct.copy buf 0 l in
        _Leaf (v, Indexed i, Hashed (Hash.extend_to_hash56 h), Indexed_and_Hashed)

    | -35l -> (* leaf whose value is in Plebeia *)
        let h = get_hash buf in
        let (bufs, _size) = Chunk.get_chunks context @@ Index.pred i in
        let v = Value.of_string @@ Chunk.string_of_cstructs bufs in
        _Leaf (v, Indexed i, Hashed (Hash.extend_to_hash56 h), Indexed_and_Hashed)

    | _ -> 
        let s_224 = Cstruct.copy buf 0 28 in
        let last_byte = Char.code @@ String.unsafe_get s_224 27 in
        match last_byte land 0x01 with
        | 1 -> (* extender *)
            (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
            let seg_code = s_224 in
            let seg = Segment_encoding.decode seg_code in
            let i' = get_index buf in
            (* We must load the child for the hash *)
            let v = parse_cell context i' in
            let h = match hash_of_view v with
              | None -> assert false
              | Some h -> h
            in
            let h = NodeHash.of_extender' ~segment_code:seg_code h in
            _Extender (seg, View v, Indexed i, Hashed h, Indexed_and_Hashed)
        | 0 -> (* internal *)
            let s_0_215 = Cstruct.copy buf 0 27 (* 216bits *) in
            let c_216_223, refer_to_right = 
              let c = Char.code @@ Cstruct.get_char buf 27 in
              (Char.chr (c land 0xfc), (c land 2) = 2)
            in
            let h = Hash.extend_to_hash56 @@ Hash.hash28_of_string (s_0_215 ^ String.make 1 c_216_223) in
            let i' = get_index buf in
            if refer_to_right then
              _Internal (Disk (Index.pred i, Maybe_Extender), Disk (i', Maybe_Extender), Indexed i, Hashed h, Indexed_and_Hashed)
            else
              _Internal (Disk (i', Maybe_Extender), Disk(Index.pred i, Maybe_Extender), Indexed i, Hashed h, Indexed_and_Hashed)
        | _ -> assert false

  (* index 32 bits (4294967296)
     block 32 bytes 
     max size of the storage 137_438_953_472 =~ 130Gb
  *)
  let index n = match index n with
    | Some i -> i
    | None -> assert false

  let bud_first_28 = String.make 28 '\255'
  let zero_24 = String.make 24 '\000'

  let write_small_leaf context v =
    let len = Value.length v in
    assert (1 <= len && len <= 32);
    let i = Context.new_index context in
    let buf = make_buf context i in
    write_string (Value.to_string v) buf 0 len

  let write_large_leaf_to_kvs context h v =
    let len = Value.length v in
    assert (len > 32);
    KVS.insert context.Context.leaf_table h v
      
  let write_large_leaf_to_plebeia context v =
    ignore @@ Chunk.write_to_chunks context 1000 (Value.to_string v)
    
  let write context n (* XXX view *) = 
    match n with
    | Internal (nl, nr, Indexed i, Hashed h, _) ->
        (* internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| *)
        let buf = make_buf context i in

        let h = Hash.to_string h in
        let il = index nl in
        let ir = index nr in
        let refer_to_left =
          if i = Index.succ il then
            (* the following index refers to the right *)
            false
          else if i = Index.succ ir then true
          else assert false
        in

        (* 0 to 215 bits *)
        Cstruct.blit_from_string h 0 buf 0 27;

        (* fix for the 223rd and 224th bits (pos 222, 223) *)
        Cstruct.set_char buf 27
          (let c = Char.code @@ String.unsafe_get h 27 in
           let c = c land 0xfc in
           Char.chr (if refer_to_left then c else c lor 2));

        (* next 32bits *)
        C.set_uint32 buf 28 (if refer_to_left then il else ir)
    
    | Bud (None, Indexed i, Hashed _, _) ->
        (* XXX No point to store the empty bud... *)
        (* empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 34 ------------------------->| *)
        let buf = make_buf context i in
        write_string bud_first_28 buf 0 28;
        set_index buf (Uint32.of_int32 (-34l))
        
    | Bud (Some n, Indexed i, Hashed _, _) ->
        (* bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 34 ------------------------->| *)
        let buf = make_buf context i in
        write_string zero_24 buf 0 24;
        C.set_uint32 buf 24 @@ index n;
        set_index buf (Uint32.of_int32 (-34l))

    | Leaf (v, Indexed i, Hashed h, _) ->
        (* leaf      |<- first 224 of hash ------------>| |<- 2^32 - 32 to 2^32 - 1 ------------->|  (may use the previous cell) *)
        (* contents are already written *)
        let len = Value.length v in
        if 1 <= len && len <= 32 then begin
          let buf = make_buf context i in
          let h = Hash.shorten_to_hash28 h in
          write_string (Hash.to_string h) buf 0 28;
          set_index buf (Uint32.of_int (len - 33)) (* 1 => -32  32 -> -1 *)
        end else begin
          let h = Hash.shorten_to_hash28 h in
          if context.store_in_leaf_table then begin
            let buf = make_buf context i in
            let h = Hash.to_string h in
            write_string h buf 0 28;
            set_index buf (Uint32.of_int32 (-33l));
          end else begin
            let buf = make_buf context i in
            let h = Hash.to_string h in
            write_string h buf 0 28;
            set_index buf (Uint32.of_int32 (-35l));
          end
        end

    | Extender (seg, n, Indexed i, Hashed _, _) ->
        (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
        let buf = make_buf context i in
        write_string (Segment_encoding.encode seg) buf 0 28;
        set_index buf @@ index n

    | (Internal (_, _, Indexed _, Not_Hashed, _)|
       Internal (_, _, (Left_Not_Indexed|Right_Not_Indexed|Not_Indexed), _, _)|
       Bud (None, Indexed _, Not_Hashed, _)|
       Bud (None, (Left_Not_Indexed|Right_Not_Indexed|Not_Indexed), _, _)|
       Bud (Some _, _, _, _)|Leaf (_, _, _, _)|Extender (_, _, _, _, _)) -> assert false

end
  
(* Read the node from context.array, parse it and create a view node with it. *)
let load_node (context : Context.t) (index : Index.t) (ewit:extender_witness) : view = 
  let v = Storage.parse_cell context index in
  match ewit, v with
  | Is_Extender, Extender _ -> v
  | Is_Extender, _ -> assert false (* better report *)
  | Maybe_Extender, Extender _ -> v
  | Not_Extender, Extender _ -> assert false (* better report *)
  | Not_Extender, _ -> v
  | Maybe_Extender, _ -> v

let () = load_node_ref := load_node

(* Recusively visit and load all the subnodes in memory.
   Only for test purposes
*)
let rec load_node_fully context n =
  let v = match n with
    | Disk (i, ewit) -> load_node context i ewit
    | View v -> v
  in
  match v with
  | Leaf _ -> View v
  | Bud (None, _, _, _) -> View v
  | Bud (Some n, i, h, x) ->
      let n = load_node_fully context n in
      View (_Bud (Some n, i, h, x))
  | Internal (n1, n2, i, h, x) ->
      let n1 = load_node_fully context n1 in
      let n2 = load_node_fully context n2 in
      View (_Internal (n1, n2, i, h, x))
  | Extender (seg, n, i, h, x) ->
      let n = load_node_fully context n in
      View (_Extender (seg, n, i, h, x))

let attach trail node context =
  (* Attaches a node to a trail even if the indexing type and hashing type is incompatible with
     the trail by tagging the modification. Extender types still have to match. *)
  match trail with
  | Top -> _Cursor (_Top, node, context)
  | Left (prev_trail, right, _, indexed_implies_hashed) ->
      _Cursor (_Left (prev_trail, right, Modified_Left, indexed_implies_hashed), node, context)
  | Right (left, prev_trail, _, indexed_implies_hashed) ->
      _Cursor (_Right (left, prev_trail, Modified_Right, indexed_implies_hashed), node, context)
  | Budded (prev_trail, _, indexed_implies_hashed) ->
      _Cursor (_Budded (prev_trail, Modified_Left, indexed_implies_hashed), node, context)
  | Extended (prev_trail, segment, _, indexed_implies_hashed) ->
      _Cursor (_Extended (prev_trail, segment, Modified_Left, indexed_implies_hashed), node, context)

module NotHashed : sig
  val leaf : Value.t -> node
  val extend : Path.segment -> node -> node
  val bud : node option -> node
  val internal : node -> node -> indexing_rule -> node
end = struct
  let leaf v = View (_Leaf (v, Not_Indexed, Not_Hashed, Not_Indexed_Any))

  let extend : Path.segment -> node -> node = fun segment node ->
    if segment = Path.empty then node
    else 
      match node with
      | View (Extender (seg, n, _, _, _)) ->
          View (_Extender (Path.concat segment seg, n, Not_Indexed, Not_Hashed, Not_Indexed_Any))
      | _ ->
          View (_Extender (segment, node, Not_Indexed, Not_Hashed, Not_Indexed_Any))
  let bud no = View (_Bud (no, Not_Indexed, Not_Hashed, Not_Indexed_Any))

  let internal n1 n2 i = 
    View (_Internal (n1, n2, i, Not_Hashed, Not_Indexed_Any))
end

let go_below_bud (Cursor (trail, n, context)) =
  (* This function expects a cursor positionned on a bud and moves it one step below. *)
  match view context n with
  | Bud (None,  _, _, _) -> Ok None
  | Bud (Some below, indexing_rule, hashed_is_transitive, indexed_implies_hashed) ->
      Ok (Some (_Cursor (
          _Budded (trail, Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed), below,  context)))
  | _ -> Error "Attempted to navigate below a bud, but got a different kind of node."

let go_side side (Cursor (trail, n, context)) =
  (* Move the cursor down left or down right in the tree, assuming we are on an internal node. *)
  match view context n with
  | Internal (l, r, indexing_rule, hashed_is_transitive, indexed_implies_hashed) ->
      Ok (match side with
          | Path.Right ->
              _Cursor (_Right (l, trail,
                             Unmodified (indexing_rule, hashed_is_transitive),
                             indexed_implies_hashed), r, context)
          | Path.Left ->
              _Cursor (_Left (trail, r,
                            Unmodified (indexing_rule, hashed_is_transitive),
                            indexed_implies_hashed), l, context))
  | _ -> Error "Attempted to navigate right or left of a non internal node"

let go_down_extender (Cursor (trail, n, context)) =
  (* Move the cursor down the extender it points to. *)
  match view context n with
  | Extender (segment, below, indexing_rule, hashed_is_transitive, indexed_implies_hashed) ->
      Ok (_Cursor (_Extended (trail, segment,
                            Unmodified (indexing_rule, hashed_is_transitive),
                            indexed_implies_hashed), below, context))
  | _ -> Error "Attempted to go down an extender but did not find an extender"

let go_up (Cursor (trail, node, context))  = match trail with
  | Top -> Error "cannot go above top"
  | Left (prev_trail, right,
          Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
      let new_node =
        View (_Internal (node, right, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Right (left, prev_trail,
           Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
      let new_node =
        View (_Internal (left, node, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Budded (prev_trail,
            Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
      let new_node =
        View (_Bud (Some node, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Extended (prev_trail, segment,
              Unmodified (indexing_rule, hashed_is_transitive), indexed_implies_hashed) ->
    let new_node =
      View (_Extender (segment, node, indexing_rule, hashed_is_transitive, indexed_implies_hashed))
    in Ok (_Cursor (prev_trail, new_node, context))

  (* Modified cases. *)

  | Left (prev_trail, right, Modified_Left, _) ->
      let internal = NotHashed.internal node right Left_Not_Indexed in
      Ok (attach prev_trail internal context)

  | Right (left, prev_trail, Modified_Right, _) ->
      let internal = NotHashed.internal left node Right_Not_Indexed in
      Ok (attach prev_trail internal context)

  | Budded (prev_trail, Modified_Left, _) ->
      let bud = NotHashed.bud @@ Some node in
      Ok (attach prev_trail bud context)

  | Extended (prev_trail, segment, Modified_Left, _) ->
      let extender = NotHashed.extend segment node in
      Ok (attach prev_trail extender context)

  | Left (_, _, Modified_Right, _)|Right (_, _, Modified_Left, _)|
    Budded (_, Modified_Right, _)|Extended (_, _, Modified_Right, _) -> assert false

let rec go_top (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> Ok c
  | _ -> go_up c >>= go_top

let parent c =
  let rec aux c =
    match c with
    | Cursor (_, Disk _, _) -> assert false (* impossible *)
    | Cursor (_, View (Bud _), _) -> Ok c (* already at the top of subtree *)
    | _ -> go_up c >>= fun c -> aux c
  in
  aux c

let unify_extenders prev_trail node context = match node with
  | Disk (_, Is_Extender) -> Error "unify_exenders: Disk is not allowed"
  | View (Extender (seg, n, _, _, _)) ->
      begin match prev_trail with
        | Extended (prev_trail', seg', _mr, _iih) ->
            Ok (attach prev_trail' (NotHashed.extend (Path.concat seg' seg) n) context)
        | _ -> Ok (attach prev_trail node context)
      end
  | _ -> Ok (attach prev_trail node context)

let rec remove_up trail context = match trail with
  | Top -> Error "cannot remove top"
  | Budded (prev_trail, _, _) ->
      Ok (_Cursor (prev_trail, NotHashed.bud None, context))
  | Extended (prev_trail, _, _, _) -> remove_up prev_trail context
  (* for Left and Right, we may need to squash Extenders in prev_trail *)
  | Left (prev_trail, right, _, _) ->
      let n = NotHashed.extend Path.(of_side_list [Right]) right in
      unify_extenders prev_trail n context
  | Right (left, prev_trail, _, _) ->
      let n = NotHashed.extend Path.(of_side_list [Left]) left in
      unify_extenders prev_trail n context

(* Let [c] is a cursor which points an Extender, whose segment is [common_prefix @ remaining_extender].
   [diverge c (common_prefix, remaining_extender, remaining_segment)] diverges a segment of [c] in the middle
   and create a path to [common_prefix @ remaining_segnet]. 
   It returns the newly created trail.
*)
let diverge (Cursor (trail, extender, _context)) (common_prefix, remaining_extender, remaining_segment) =
  match extender with
  | View (Extender (_seg, n, _ir, _hit, _iih)) -> (* _seg = common_prefix @ remaining_extender *)
      begin match Path.cut remaining_segment, Path.cut remaining_extender with
        | None, _ -> Error "diverge: remaining_segment is empty"
        | _, None -> Error "diverge: remaining_extender is empty"
        | Some (side, seg), Some (side', seg') -> 
            (* go down along common_prefix *)
            assert (side <> side');
            let trail = 
              if Path.is_empty common_prefix then trail 
              else _Extended (trail, common_prefix, Modified_Left, Not_Indexed_Any)
            in
            let n' = NotHashed.extend seg' n in
            match side with
            | Path.Left -> 
                if Path.is_empty seg then
                  Ok (_Left (trail, n', Modified_Left, Not_Indexed_Any))
                else
                  Ok (_Extended (_Left (trail, n', Modified_Left, Not_Indexed_Any),
                            seg, Modified_Left, Not_Indexed_Any))
            | Path.Right -> 
                if Path.is_empty seg then
                  Ok (_Right (n', trail, Modified_Right, Not_Indexed_Any))
                else
                  Ok (_Extended (_Right (n', trail, Modified_Right, Not_Indexed_Any),
                            seg, Modified_Left, Not_Indexed_Any))
      end
  | _ -> Error "diverge: not an Extender"

(* Follow the segment from the cursor. If the segment terminates 
  or diverges in the middle of an extender, it returns the common prefix
  information. 
*)
let access_gen cur segment =
  (* returns the cursor found by following the segment from the given cursor *)
  let rec aux (Cursor (trail, n, context) as cur) segment =
    let v = view context n in
    match Path.cut segment with
    | None -> Ok (cur, None)
    | Some (dir, segment_rest) ->
        match v with
        | Leaf _ -> Error "Reached a leaf before finishing"
        | Bud _ ->  Error "Reached a bud before finishing"
        | Internal (l, r,
                    internal_node_indexing_rule,
                    hashed_is_transitive,
                    indexed_implies_hashed) -> begin
            match dir with
            | Left ->
              let new_trail =
                _Left (
                  trail, r,
                  Unmodified (
                    internal_node_indexing_rule,
                    hashed_is_transitive),
                  indexed_implies_hashed) in
              aux (_Cursor (new_trail, l, context)) segment_rest
            | Right ->
              let new_trail = 
                _Right (
                  l, trail,
                  Unmodified (
                    internal_node_indexing_rule, 
                    hashed_is_transitive),
                  indexed_implies_hashed) in
              aux (_Cursor (new_trail, r, context)) segment_rest
          end
        | Extender (extender, node_below,
                    indexing_rule,
                    hashed_is_transitive,
                    indexed_implies_hashed) ->
          let (_, remaining_extender, remaining_segment) as common_prefix =
            Path.common_prefix extender segment in
          if remaining_extender = Path.empty then
            let new_trail =
              _Extended (trail, extender,
                       Unmodified (
                         indexing_rule,
                         hashed_is_transitive),
                       indexed_implies_hashed) in
            aux (_Cursor (new_trail, node_below, context)) remaining_segment
          else
            if remaining_segment = Path.empty then
              Error "Finished in the middle of an Extender"
            else
              (* diverge *)
              Ok (cur, Some common_prefix)
  in
  aux cur segment

let subtree cur segment =
  go_below_bud cur >>= function
  | None -> Error "Nothing beneath this bud"
  | Some cur ->
      access_gen cur segment >>= function 
      | (_, Some _) -> Error "Terminated or diverged in the middle of an Extender"
      | (Cursor (trail, n, context), None) -> 
          match view context n with
          | Bud _ as v -> Ok (_Cursor (trail, View v, context))
          | _ -> Error "Reached to a non Bud"

let get cur segment = 
  go_below_bud cur >>= function
  | None -> Error "Nothing beneath this bud"
  | Some cur ->
      access_gen cur segment >>= function 
      | (_, Some _) -> Error "Terminated or diverged in the middle of an Extender"
      | (Cursor (_, n, context), None) -> 
          match view context n with
          | Leaf (v, _, _, _) -> Ok v
          | _ -> Error "Reached to a non Leaf"

let empty context =
  (* A bud with nothing underneath, i.e. an empty tree or an empty sub-tree. *)
  _Cursor (_Top, NotHashed.bud None, context)

let delete cur segment =
  go_below_bud cur >>= function
  | None -> Error "Nothing beneath this bud"
  | Some cur ->
      access_gen cur segment >>= function 
      | (_, Some _) -> Error "Terminated or diverged in the middle of an Extender"
      | (Cursor (trail, _n, context), None) -> 
          remove_up trail context 
          >>= parent 

let alter (Cursor (trail, _, context) as cur) segment alteration =
  (* XXX 2 cases. not cool *)
  go_below_bud cur >>= function
  | None ->
      alteration None >>= fun n' ->
      let n' = NotHashed.extend segment n' in
      let n' = NotHashed.bud (Some n') in
      Ok (_Cursor (trail, n', context))
  | Some cur -> 
      access_gen cur segment >>= fun (c,segsopt) ->
      begin match segsopt with
        | None -> 
            (* Should we view the node? *)
            let Cursor (trail, n, context) = c in
            let v = view context n in
            Ok (trail, Some v)
        | Some segs -> diverge c segs >>| fun trail -> (trail, None)
      end >>= fun (trail, nopt) ->
      alteration nopt >>= fun n -> 
      parent @@ attach trail n context

let upsert cur segment value =
  alter cur segment (fun x ->
     let y = Ok (NotHashed.leaf value) in 
     match x with
     | None -> y
     | Some (Leaf _) -> y
     | Some _ -> Error "a non Leaf node already present for this path")

let insert cur segment value =
  alter cur segment (function
      | None -> Ok (View (_Leaf (value, Not_Indexed, Not_Hashed, Not_Indexed_Any)))
      | Some _ -> Error "a node already present for this path")

let create_subtree cur segment =
  alter cur segment (function
      | None -> Ok (NotHashed.bud None)
      | Some _ -> Error "a node already present for this path")


let hash (Cursor (trail, node, context)) =
  let rec hash_aux : node -> (node * hash) = function
    | Disk (index, wit) -> 
        let v, h = hash_aux' (load_node context index wit) in View v, h
    | View v -> 
        let v, h = hash_aux' v in View v, h

  and hash_aux' : view -> (view * hash) = fun v -> 
    match v with
    (* easy case where it's already hashed *)
    | Leaf (_, _, Hashed h, _) -> (v, h)
    | Bud (_, _, Hashed h, _) -> (v, h)
    | Internal (_, _, _, Hashed h, _)  -> (v, h)
    | Extender (_, _, _, Hashed h, _) -> (v, h)

    (* hashing is necessary below *)
    | Leaf (v, _, Not_Hashed, _) -> 
        let h = NodeHash.of_leaf v in
        (_Leaf (v, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Bud (Some underneath, _, Not_Hashed, _) ->
        let (node, h) = hash_aux underneath in
        (_Bud (Some node, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Bud (None, _, Not_Hashed, _) ->
        let h = NodeHash.of_empty_bud in
        (_Bud (None, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Internal (left, right, Left_Not_Indexed, Not_Hashed, _) -> (
        let (left, hl) = hash_aux left
        and (right, hr) = hash_aux right in
        (*
           |<-     H(0x01 || l || h)    ->|
           |                           |00|0...........................01|
        *)
        let h = NodeHash.of_internal hl hr in
        (_Internal (left, right, Left_Not_Indexed, Hashed h, Not_Indexed_Any), h))

    | Internal (left, right, Right_Not_Indexed, Not_Hashed, _) -> (
        let (left, hl) = hash_aux left
        and (right, hr) = hash_aux right in
        (*
           |<-     H(0x01 || l || h)    ->|
           |                           |00|0...........................01|
        *)
        let h = NodeHash.of_internal hl hr in
        (_Internal (left, right, Right_Not_Indexed, Hashed h, Not_Indexed_Any), h))

    | Extender (segment, underneath, _, Not_Hashed, _)  ->
        let (underneath, h) = hash_aux underneath in
        (*
           |<-                       H_child                           ->|
           | The first 224bits of H_child |0......01|<- segment bits ->|1|
        *) 
        let h = NodeHash.of_extender segment h in
        (_Extender (segment, underneath, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Internal (_, _, (Not_Indexed|Indexed _), Not_Hashed, _) -> assert false
  in 
  let (node, h) =  hash_aux node in
  (_Cursor (trail, node, context), h)

(* XXX Operations are NOT atomic at all *)
let commit_node context node =
  let rec commit_aux : node -> (node * Index.t * hash) = function
    | Disk (index, wit) ->
        let v, i, h = commit_aux' (load_node context index wit) in
        View v, i, h
    | View v -> 
        let v, i, h = commit_aux' v in
        View v, i, h

  and commit_aux' : view -> (view * Index.t * hash) = fun v -> 
    match v with
    (* easy case where it's already commited *)
    | Leaf (_, Indexed i, Hashed h, _) -> (v, i, h)
    | Bud (_, Indexed i, Hashed h, _) -> (v, i, h)
    | Internal (_, _, Indexed i, Hashed h, _)  -> (v, i, h)
    | Extender (_, _, Indexed i, Hashed h, _) -> (v, i, h)

    (* indexing is necessary below.  If required, the hash is also computed *)
    | Leaf (value, Not_Indexed, h, _) ->
        let h = match h with
          | Not_Hashed -> NodeHash.of_leaf value 
          | Hashed h -> h
        in
        (* if the size of the value is 1 <= size <= 32, the contents are written
           to the previous index of the leaf *)
        let len = Value.length value in
        if 1 <= len && len <= 32 then begin
          Storage.write_small_leaf context value;
          let i = Context.new_index context in
          let v = _Leaf (value, Indexed i, Hashed h, Indexed_and_Hashed) in
          Storage.write context v;
          (v, i, h)
        end else 
          if context.Context.store_in_leaf_table then begin
            let h28 = Hash.shorten_to_hash28 h in
            Storage.write_large_leaf_to_kvs context h28 value;
            let i = Context.new_index context in
            let v = _Leaf (value, Indexed i, Hashed h, Indexed_and_Hashed) in
            Storage.write context v;
            (v, i, h)
          end else begin
            Storage.write_large_leaf_to_plebeia context value;
            let i = Context.new_index context in
            let v = _Leaf (value, Indexed i, Hashed h, Indexed_and_Hashed) in
            Storage.write context v;
            (v, i, h)
          end
        
    | Bud (Some underneath, Not_Indexed, h, _) ->
        let (node, _, h') = commit_aux underneath in
        let h = match h with
          | Hashed h -> assert (h = h'); h
          | _ -> NodeHash.of_bud (Some h')
        in
        let i = Context.new_index context in
        let v = _Bud (Some node, Indexed i, Hashed h, Indexed_and_Hashed) in
        Storage.write context v;
        (v, i, h)

    | Bud (None, Not_Indexed, h, _) ->
        begin match h with
          | Hashed h -> assert (h = NodeHash.of_empty_bud)
          | _ -> ()
        end;
        let i = Context.new_index context in
        let v = _Bud (None, Indexed i, Hashed NodeHash.of_empty_bud, Indexed_and_Hashed) in
        Storage.write context v;
        (v, i, NodeHash.of_empty_bud)

    | Internal (left, right, Left_Not_Indexed, h, _) ->
        let (right, _ir, hr) = commit_aux right in
        let (left, _il, hl) = commit_aux left in (* This one must be the latter *)
        let h = match h with
          | Not_Hashed -> NodeHash.of_internal hl hr
          | Hashed h -> h
        in
        let i = Context.new_index context in
        let v = _Internal (left, right, Indexed i, Hashed h, Indexed_and_Hashed) in
        Storage.write context v;
        (v, i, h)

    | Internal (left, right, Right_Not_Indexed, h, _) ->
        let (left, _il, hl) = commit_aux left in
        let (right, _ir, hr) = commit_aux right in (* This one must be the latter *)
        let h = match h with
          | Not_Hashed -> NodeHash.of_internal hl hr
          | Hashed h -> h
        in
        let i = Context.new_index context in
        let v = _Internal (left, right, Indexed i, Hashed h, Indexed_and_Hashed) in
        Storage.write context v;
        (v, i, h)

    | Extender (segment, underneath, Not_Indexed, h, _)  ->
        let (underneath, _i, h') = commit_aux underneath in
        let h = match h with
          | Hashed h -> h
          | Not_Hashed -> NodeHash.of_extender segment h'
        in
        let i = Context.new_index context in
        let v = _Extender (segment, underneath, Indexed i, Hashed h, Indexed_and_Hashed) in
        Storage.write context v;
        (v, i, h)
        
    | (Leaf (_, Left_Not_Indexed, _, _)|
       Leaf (_, Right_Not_Indexed, _, _)|
       Bud (Some _, Left_Not_Indexed, _, _)|
       Bud (Some _, Right_Not_Indexed, _, _)|
       Bud (None, Left_Not_Indexed, _, _)|
       Bud (None, Right_Not_Indexed, _, _)|
       Internal (_, _, Not_Indexed, _, _)|
       Extender (_, _, Left_Not_Indexed, _, _)|
       Extender (_, _, Right_Not_Indexed, _, _)) -> assert false

    | (Leaf (_, Indexed _, Not_Hashed, _)|Bud (None, Indexed _, Not_Hashed, _)|
       Bud (Some _, Indexed _, Not_Hashed, _)|
       Internal (_, _, Indexed _, Not_Hashed, _)|
       Extender (_, _, Indexed _, Not_Hashed, _)) -> assert false

  in 
  let (node, i, h) =  commit_aux node in
  node, i, h

let to_disk context n =
  let n, i, h = commit_node context n in
  match n with
  | Disk _ -> n, h
  | View (Extender _) -> Disk (i, Is_Extender), h
  | View _ -> Disk (i, Not_Extender), h
  
let commit (Cursor (trail, node, context)) =
  let (node, i, h) =  commit_node context node in
  (_Cursor (trail, node, context), i, h)

let commit (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> 
      let (Cursor (_ , _, context) as c), i, h = commit c in
      begin match Hashtbl.find_opt context.Context.roots_table h with
        | Some i' -> Error (c, i, h, i') (* Hash collision *)
        | None -> Hashtbl.add context.Context.roots_table h i; Ok (c, h)
      end
        
  | _ -> failwith "commit: cursor must point to a root"

let snapshot _ _ _ = failwith "not implemented"
let gc ~src:_ _ ~dest:_ = failwith "not implemented"

let open_context ~filename:_ = failwith "not implemented"
let root context h = 
  match Hashtbl.find_opt context.Context.roots_table h with
  | None -> Error "The root is not found in the roots_table"
  | Some i -> Ok (_Cursor( _Top, Disk(i, Not_Extender), context))

type where_from =
  | Down_to of dir
  | Up_from of dir

and dir =
  | Left
  | Right
  | Center

let from_Some = function
  | Some x -> x
  | None -> assert false

let rec traverse log rev_path (Cursor (trail, n, context)) = 
  let v = match n with
    | Disk (i, ewit) -> load_node context i ewit
    | View v -> v
  in
  let c = _Cursor (trail, View v, context) in
  match v, log, rev_path with
  | Leaf _, [], _ -> c
  | Leaf _, Down_to d :: log', _ :: rev_path -> 
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Bud (None, _, _, _), [], _ -> c
  | Bud (None, _, _, _), Down_to d :: log', rev_path -> 
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Bud (Some _, _, _, _), Up_from _ :: [], _ -> c
  | Bud (Some _, _, _, _), Up_from _ :: Down_to d :: log', _ :: rev_path ->
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Bud (Some _, _, _, _), (Down_to _ :: _ | []), rev_path ->
      let c = from_Some @@ from_Ok @@ go_below_bud c in
      traverse (Down_to Center :: log) rev_path c
  | Internal _, (Down_to _ :: _ | []), rev_path ->
      let c = from_Ok @@ go_side Path.Left c in
      traverse (Down_to Left :: log) (Path.Left :: rev_path) c
  | Internal _, Up_from Left :: log', rev_path ->
      let c = from_Ok @@ go_side Path.Right c in
      traverse (Down_to Right :: log') (Path.Right :: rev_path) c
  | Internal _, Up_from Right :: [], _ -> c
  | Internal _, Up_from Right :: Down_to d :: log', _ :: rev_path ->
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | Extender (seg, _, _, _, _), (Down_to _ :: _ | []), rev_path ->
      let c = from_Ok @@ go_down_extender c in
      traverse (Down_to Center :: log) (List.rev_append (seg :> Path.side list) rev_path) c
  | Extender (_, _, _, _, _), Up_from _ :: [], _ -> c
  | Extender (seg, _, _, _, _), Up_from _ :: Down_to d :: log', rev_path ->
      let rev_path =
        let rec f seg rev_path = match seg, rev_path with
          | s::seg, s'::rev_path -> assert (s = s');  f seg rev_path
          | [], _ -> rev_path
          | _, [] -> assert false
        in
        f (seg :> Path.side list) rev_path
      in
      let c = from_Ok @@ go_up c in
      traverse (Up_from d :: log') rev_path c
  | _ -> assert false
