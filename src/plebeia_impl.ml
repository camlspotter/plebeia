(** Implementation of space-efficient binary Patricia trees in OCaml.
    The implementation is geared for used in Tezos, though it is rather
    generic. A stop-and-copy GC is provided. This implementation aims
    to maximize correctness and cares second about efficiency. Extracting
    and efficient C program from F* should be explored. *)

(* Hash

     leaf      |<-      H(0x00 || v)        ->|
               |                              |0...........................01|

     bud       |<------------------ hash of its child ---------------------->|

     empty bud |0000000000000000000000000000000000000000000000000000000000000|
  
     internal  |<-     H(0x01 || l || h)    ->|
               |                            |0|0...........................01|
  
     extender  |<-                       H_child                           ->|
               | The first 224bits of H_child |0*1|<- segment bits ------->|1|


  Storage

               |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
     ----------------------------------------------------------------------------------------
     internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->|
     extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
     leaf      |<- first 224 of hash ------------>| |<- 2^32 - 32 to 2^32 - 1 ------------->|  (may use the previous cell)
     bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 33 ------------------------->|
     empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 33 ------------------------->|
     
     index of the child must be < 2^32 - 33
*)
     
open Error

type error = string
type value = Value.t
type segment = Path.segment
type hash = Hash.t
type index = int64 (* XXX since the size of the index in the storage is 32bits, we should use uint32 *)

type indexed_implies_hashed =
  | Indexed_and_Hashed
  | Not_Indexed_Any
  (* Type used to prove that all indexed nodes have been hashed. *)

type hashed_is_transitive =
  | Hashed of Hash.t
  | Not_Hashed
  (* Type used to prove that if a node is hashed then so are its children.
     The type also provides the hash as a witness.*)

type internal
type not_internal

type indexing_rule =
  | Indexed of int64
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
  | Hashed of Hash.t
  | Not_Hashed

type indexed_witness =
  | Indexed of int64
  | Not_Indexed

module Context = struct
  type t = {
    array : Bigstring.t ;
    (* mmaped array where the nodes are written and indexed. *)
    mutable length : int64 ;
    (* Current length of the node table. *)
    leaf_table  : KVS.t ;
    (* Hash table  mapping leaf hashes to their values. *)
    roots_table : (Hash.t, int64) Hashtbl.t ;
    (* Hash table mapping root hashes to indices in the array. *)
    fd : Unix.file_descr ; 
    (* File descriptor to the mapped file *)
  }

  let make ?pos ?(shared=false) ?(length=(-1)) fn =
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
      length = 0L ;
      leaf_table = KVS.make () ;
      roots_table = Hashtbl.create 1 ;
      fd = fd ;
    }

  let new_index c =
    let i = Int64.add c.length 1L in
    c.length <- i;
    i
    
  let free { fd ; _ } = Unix.close fd
end

module PrivateNode : sig 

  type node =
    | Disk of int64 * extender_witness
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
  val index : node -> int64 option
  val hashed : node -> bool
  val hash_of_view : view -> Hash.t option

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

  val load_node : Context.t -> int64 -> extender_witness -> view

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
    | Disk of int64 * extender_witness
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
              let open Int64 in
              if (sub i li = 1L) || (sub i ri = 1L) then Ok ()
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

  let load_node (_context : Context.t) (_index : int64) (_ewit:extender_witness) : view = 
    failwith "not implemented"
  (* Read the node from context.array, parse it and create a view node with it. *)

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

module Storage = struct
  (** node storage *)

  module C = Cstruct.LE (* XXX dunno which tezos uses... *)

  (*           |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
     internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->|
     extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
     leaf      |<- first 224 of hash ------------>| |<- 2^32 - 32 to 2^32 - 1 ------------->|  (use the previous cell)
     leaf      |<- first 224 of hash ------------>| |<- 2^32 - 33 ------------------------->|  (use KVS)
     bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 34 ------------------------->|
     empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 34 ------------------------->|
     
     index of the child must be < 2^32 - 34   XXX no check is implemented yet
  *)     

  let make_buf context i =
    let i = Int64.to_int i in (* XXX check overflow *)
    Cstruct.of_bigarray ~off:(i*32) ~len:32 context.Context.array

  let int64_of_uint32_encoded_in_int32 x =
    if x >= 0l then Int64.of_int32 x
    else Int64.(add (of_int32 x) 2147483647L (* Int32.max_int *)) (* XXX check! *)
    
  let rec parse_cell context i =
    let buf = make_buf context i in
    match C.get_uint32 buf 28 (* from 224th bit *) with
    | -34l -> (* bud *)
        begin match Cstruct.get_char buf 0 with
          | '\255' -> _Bud (None, Indexed i, Hashed Hash.of_empty_bud, Indexed_and_Hashed)
          | _ ->  
              (* We must load the child for the hash *)
              let i' = int64_of_uint32_encoded_in_int32 @@ C.get_uint32 buf 24 in
              let v = parse_cell context i' in
              begin match hash_of_view v with
              | None -> assert false
              | Some h -> _Bud (Some (View v), Indexed i, Hashed h, Indexed_and_Hashed)
              end
        end
    | -33l -> (* leaf whose value is in the KVS *)
        let h = Hash.value_hash_of_string @@ Cstruct.copy buf 0 28 in
        begin match KVS.get_opt context.Context.leaf_table h with
          | None -> assert false (* ERROR! *)
          | Some v -> _Leaf (v, Indexed i, Hashed (Hash.of_value_hash h), Indexed_and_Hashed)
        end
    | x when -32l <= x && x <= -1l -> (* leaf whose value is in the previous cell *)
        let l = 33 + Int32.to_int x in (* 1 to 32 *)
        let h = Hash.value_hash_of_string @@ Cstruct.copy buf 0 28 in
        let buf = make_buf context (Int64.sub i 1L) in
        let v = Value.of_string @@ Cstruct.copy buf 0 l in
        _Leaf (v, Indexed i, Hashed (Hash.of_value_hash h), Indexed_and_Hashed)
    | _ -> 
        let s_224 = Cstruct.copy buf 0 28 in
        let last_byte = Char.code @@ String.unsafe_get s_224 27 in
        match last_byte land 0x01 with
        | 1 -> (* extender *)
            (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
            let seg_code = s_224 in
            let seg = Hash.decode_segment seg_code in
            let i' = int64_of_uint32_encoded_in_int32 @@ C.get_uint32 buf 28 in
            (* We must load the child for the hash *)
            let v = parse_cell context i' in
            let h = match hash_of_view v with
              | None -> assert false
              | Some h -> h
            in
            let h = Hash.of_extender' ~segment_code:seg_code h in
            _Extender (seg, View v, Indexed i, Hashed h, Indexed_and_Hashed)
        | 0 -> (* internal *)
            let s_0_215 = Cstruct.copy buf 0 27 (* 216bits *) in
            let c_216_223, refer_to_right = 
              let c = Char.code @@ Cstruct.get_char buf 27 in
              (Char.chr (c land 0xfc), (c land 2) = 2)
            in
            let h = Hash.of_value_hash (Hash.value_hash_of_string (s_0_215 ^ String.make 1 c_216_223)) in
            let i' = int64_of_uint32_encoded_in_int32 @@ C.get_uint32 buf 28 in
            if refer_to_right then
              _Internal (Disk (Int64.sub i 1L, Maybe_Extender), Disk (i', Maybe_Extender), Indexed i, Hashed h, Indexed_and_Hashed)
            else
              _Internal (Disk (i', Maybe_Extender), Disk(Int64.sub i 1L, Maybe_Extender), Indexed i, Hashed h, Indexed_and_Hashed)
        | _ -> assert false
    
  (* index 32 bits (4294967296)
     block 32 bytes 
     max size of the storage 137_438_953_472 =~ 130Gb
     
     internal node and bud are not distinguishable!!
  *)
  let index n = match index n with
    | Some i -> i
    | None -> assert false

  let bud_first_28 = String.make 28 '\255'
  let zero_24 = String.make 24 '\000'
    
  let write context = function
    | Internal (nl, nr, Indexed i, Hashed h, _) ->
        (* internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| *)
        let buf = make_buf context i in

        let h = Hash.to_string h in
        let il = index nl in
        let ir = index nr in
        let refer_to_left =
          if i = Int64.(add il 1L) then
            (* the following refers to the right *)
            false
          else if i = Int64.(add ir 1L) then true
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
        Cstruct.LE.set_uint32 buf 28 (Int64.to_int32 @@ if refer_to_left then il else ir) (* XXX Check overflow *)
    
    | Bud (None, Indexed i, Hashed _, _) ->
        (* XXX No point to store the empty bud... *)
        (* empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 33 ------------------------->| *)
        let buf = make_buf context i in
        Cstruct.blit_from_string bud_first_28 0 buf 0 28;
        C.set_uint32 buf 28 (-33l)
        
    | Bud (Some _, Indexed i, Hashed _, _) ->
        (* bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 33 ------------------------->| *)
        let buf = make_buf context i in
        Cstruct.blit_from_string zero_24 0 buf 0 24;
        C.set_uint32 buf 24 (Int64.to_int32 i);
        C.set_uint32 buf 28 (-33l)

    | Leaf (v, Indexed i, Hashed h, _) ->
        (* leaf      |<- first 224 of hash ------------>| |<- 2^32 - 32 to 2^32 - 1 ------------->|  (may use the previous cell) *)
        let len = Value.length v in
        if 1 <= len && len <= 32 then begin
          begin
            (* leaf whose value is in the previous cell *)
            (* assuming i-1 is reserved for the value *)
            let buf' = make_buf context (Int64.sub i 1L) in
            Cstruct.blit_from_string (Value.to_string v) 0 buf' 0 len;
          end;
          let buf = make_buf context i in
          Cstruct.blit_from_string (Hash.to_string h) 0 buf 0 28;
          C.set_uint32 buf 28 (Int32.of_int (len - 33)) (* 1 => -32  32 -> -1 *)
        end else begin
          KVS.insert context.leaf_table (Hash.to_value_hash h) v;
          let buf = make_buf context i in
          Cstruct.blit_from_string (Hash.to_string h) 0 buf 0 28;
          C.set_uint32 buf 28 (-33l);
        end

    | Extender (seg, _, Indexed i, Hashed _, _) ->
        (* extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>| *)
        let buf = make_buf context i in
        Cstruct.blit_from_string (Hash.encode_segment seg) 0 buf 0 28;
        C.set_uint32 buf 28 (Int64.to_int32 i)

    | (Internal (_, _, Indexed _, Not_Hashed, _)|
       Internal (_, _, (Left_Not_Indexed|Right_Not_Indexed|Not_Indexed), _, _)|
       Bud (None, Indexed _, Not_Hashed, _)|
       Bud (None, (Left_Not_Indexed|Right_Not_Indexed|Not_Indexed), _, _)|
       Bud (Some _, _, _, _)|Leaf (_, _, _, _)|Extender (_, _, _, _, _)) -> assert false

end
  
let view context node = match node with
  | Disk (i, wit) -> load_node context i wit
  | View v -> v

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
    let vnode = view context n in
    match Path.cut segment with
    | None -> Ok (cur, None)
    | Some (dir, segment_rest) ->
        match vnode with
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
            Ok (trail, Some (view context n))
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
    | View vnode -> 
        let v, h = hash_aux' vnode in View v, h

  and hash_aux' : view -> (view * hash) = fun vnode -> 
    match vnode with
    (* easy case where it's already hashed *)
    | Leaf (_, _, Hashed h, _) -> (vnode, h)
    | Bud (_, _, Hashed h, _) -> (vnode, h)
    | Internal (_, _, _, Hashed h, _)  -> (vnode, h)
    | Extender (_, _, _, Hashed h, _) -> (vnode, h)

    (* hashing is necessary below *)
    | Leaf (value, _, Not_Hashed, _) ->
        let h = Hash.of_leaf value in
        (_Leaf (value, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Bud (Some underneath, _, Not_Hashed, _) ->
        let (node, h) = hash_aux underneath in
        let h = Hash.of_bud (Some h) in
        (_Bud (Some node, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Bud (None, _, Not_Hashed, _) ->
        (_Bud (None, Not_Indexed, Hashed Hash.of_empty_bud, Not_Indexed_Any), Hash.of_empty_bud)

    | Internal (left, right, Left_Not_Indexed, Not_Hashed, _) -> (
        let (left, hl) = hash_aux left and (right, hr) = hash_aux right in
        let h = Hash.of_internal_node hl hr in
        (_Internal (left, right, Left_Not_Indexed, Hashed h, Not_Indexed_Any), h))

    | Internal (left, right, Right_Not_Indexed, Not_Hashed, _) -> (
        let (left, hl) = hash_aux left and (right, hr) = hash_aux right in
        let h = Hash.of_internal_node hl hr in
        (_Internal (left, right, Right_Not_Indexed, Hashed h, Not_Indexed_Any), h))

    | Extender (segment, underneath, _, Not_Hashed, _)  ->
        let (underneath, h) = hash_aux underneath in
        let h = Hash.of_extender segment h in
        (_Extender (segment, underneath, Not_Indexed, Hashed h, Not_Indexed_Any), h)
 
    | Internal (_, _, (Not_Indexed|Indexed _), Not_Hashed, _) -> assert false
  in 
  let (node, h) =  hash_aux node in
  (_Cursor (trail, node, context), h)

(* XXX Operations are NOT atomic at all *)
let commit (Cursor (trail, node, context)) =
  let rec commit_aux : node -> (node * index * hash) = function
    | Disk (index, wit) ->
        let v, i, h = commit_aux' (load_node context index wit) in
        View v, i, h
    | View vnode -> 
        let v, i, h = commit_aux' vnode in
        View v, i, h

  and commit_aux' : view -> (view * index * hash) = fun vnode -> 
    match vnode with
    (* easy case where it's already commited *)
    | Leaf (_, Indexed i, Hashed h, _) -> (vnode, i, h)
    | Bud (_, Indexed i, Hashed h, _) -> (vnode, i, h)
    | Internal (_, _, Indexed i, Hashed h, _)  -> (vnode, i, h)
    | Extender (_, _, Indexed i, Hashed h, _) -> (vnode, i, h)

    (* indexing is necessary below.  If required, the hash is also computed *)
    | Leaf (value, Not_Indexed, h, _) ->
        let h = match h with
          | Not_Hashed -> Hash.of_leaf value 
          | Hashed h -> h
        in
        (* if the size of the value is 1 <= size <= 32, the contents are written
           to the previous index of the leaf *)
        let len = Value.length value in
        if 1 <= len && len <= 32 then ignore (Context.new_index context);

        let i = Context.new_index context in
        let v = _Leaf (value, Indexed i, Hashed h, Not_Indexed_Any) in
        Storage.write context v;
        (v, i, h)
        
    | Bud (Some underneath, Not_Indexed, h, _) ->
        let (node, _, h') = commit_aux underneath in
        let h = match h with
          | Hashed h -> assert (h = h'); h
          | _ -> Hash.of_bud (Some h')
        in
        let i = Context.new_index context in
        let v = _Bud (Some node, Indexed i, Hashed h, Not_Indexed_Any) in
        Storage.write context v;
        (v, i, h)

    | Bud (None, Not_Indexed, h, _) ->
        begin match h with
          | Hashed h -> assert (h = Hash.of_empty_bud)
          | _ -> ()
        end;
        let i = Context.new_index context in
        let v = _Bud (None, Indexed i, Hashed Hash.of_empty_bud, Not_Indexed_Any) in
        Storage.write context v;
        (v, i, Hash.of_empty_bud)

    | Internal (left, right, Left_Not_Indexed, h, _) ->
        let (right, _ir, hr) = commit_aux right in
        let (left, _il, hl) = commit_aux left in (* This one must be the latter *)
        let h = match h with
          | Not_Hashed -> Hash.of_internal_node hl hr
          | Hashed h -> h
        in
        let i = Context.new_index context in
        let v = _Internal (left, right, Indexed i, Hashed h, Not_Indexed_Any) in
        Storage.write context v;
        (v, i, h)

    | Internal (left, right, Right_Not_Indexed, h, _) ->
        let (left, _il, hl) = commit_aux left in
        let (right, _ir, hr) = commit_aux right in (* This one must be the latter *)
        let h = match h with
          | Not_Hashed -> Hash.of_internal_node hl hr
          | Hashed h -> h
        in
        let i = Context.new_index context in
        let v = _Internal (left, right, Indexed i, Hashed h, Not_Indexed_Any) in
        Storage.write context v;
        (v, i, h)

    | Extender (segment, underneath, Not_Indexed, h, _)  ->
        let (underneath, _i, h') = commit_aux underneath in
        let h = match h with
          | Hashed h -> h
          | Not_Hashed -> Hash.of_extender segment h'
        in
        let i = Context.new_index context in
        let v = _Extender (segment, underneath, Indexed i, Hashed h, Not_Indexed_Any) in
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
  let (node, _, h) =  commit_aux node in
  (_Cursor (trail, node, context), h)

let commit (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> commit c
  | _ -> failwith "commit: cursor must point to a root"
           
let snapshot _ _ _ = failwith "not implemented"
let update _ _ _ = failwith "not implemented"
let gc ~src:_ _ ~dest:_ = failwith "not implemented"

let open_context ~filename:_ = failwith "not implemented"
let root _ _ = failwith "not implemented"
