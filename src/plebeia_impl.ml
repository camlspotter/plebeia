(** Implementation of space-efficient binary Patricia trees in OCaml.
    The implementation is geared for used in Tezos, though it is rather
    generic. A stop-and-copy GC is provided. This implementation aims
    to maximize correctness and cares second about efficiency. Extracting
    and efficient C program from F* should be explored. *)

open Error

let failwithf fmt = Printf.ksprintf failwith fmt
                  
module Path : sig
  (** A module encapsulating the concept of a path through the Patricia tree.
      A path is a sequence of n full segments. The n-1 first segments end in
      a bud and the nth ends in a leaf. Internal segments are bit of paths
      encoded in the internal nodes of the Patricia tree while tip segments
      represent the bits of path encoded close to the leaf. *)

  type side = Left | Right
  (** Binary tree, by convention when a bool or a bit is used, 0 = false = Left
      and 1 = true = Right *)

  val dummy_side : side
  (** Whenever we need to consistently pick side that doesn't really correspond
    to a real side. By convention, it is Left. *)

  type segment = private side list
  (** A segment is always just a list of sides. TODO: use bit arithmetic for
      speed and memory footprint.*)

  val empty : segment
  (** The empty segment. *)

  val is_empty : segment -> bool

  val cut : segment -> (side * segment) option
  (** Cuts a path into a head and tail if not empty. *)

  val common_prefix : segment -> segment -> segment * segment * segment
  (** Factors a common prefix between two segments. *)

  val of_side_list : side list -> segment
  (** Converts a list of side to a segment. *)

  val to_string : segment -> string
  (** String representation of a segment, e.g. "LLLRLLRL" *)

  val of_string : string -> segment option
  (** Parse the string representation of a segment, e.g. "LLRLLRL" *)

  val (@) : segment -> segment -> segment

end = struct
  type side = Left | Right
  let dummy_side = Left
  type segment = side list (* FIXME: use bit arithmetic *)
  let is_empty x = x = []
  let cut = function
    | [] -> None
    | h::t -> Some (h,t)
  let empty = []

  let (@) = (@)

  let to_string s =
    String.concat "" (List.map (function Left -> "L" | Right -> "R") s)

  let of_string s =
    let rec aux st = function
      | -1 -> Some st
      | n -> 
          match String.unsafe_get s n with
          | 'L' -> aux (Left :: st) (n-1)
          | 'R' -> aux (Right :: st) (n-1)
          | _ -> None
    in
    aux [] @@ String.length s - 1

  let rec common_prefix seg1 seg2 = match (seg1, seg2) with
    | (h1 :: t1, h2 :: t2) ->
      if h1 = h2 then
        let (prefix, r1, r2) = common_prefix t1 t2 in
        (h1 :: prefix, r1, r2)
      else
        ([], seg1, seg2)
    | ([], _) -> ([], [], seg2)
    | (_, []) -> ([], seg1, [])

  let of_side_list l = l
end

module Value : sig
  (** Module encapsulating the values inserted in the Patricia tree. *)

  type t = private string
  (** Type of a value. *)

  val of_string : string -> t

  val to_string : t -> string

end = struct
  type t = string
  let of_string s = s
  let to_string s = s
end

module Hash : sig
  type t = private string
  (** Type for the hash.  448 bits *)

  val to_string : t -> string
  val to_hex : t -> string

  val of_string : string -> t
    
  val of_leaf : Value.t -> t
  val of_empty_bud : t
  val of_bud : t option -> t
  val of_internal_node : t -> t -> t
  val of_extender : Path.segment -> t -> t
    
  val hash_of_segment : Path.segment -> string
  val segment_of_hash : string -> Path.segment
end = struct

  module Blake2B_28 = struct
    let of_string s =
      let open Blake2.Blake2b in
      let b = init 28 in
      update b (Bigstring.of_string s);
      let Hash bs = final b in
      Bigstring.to_string bs
        
    let of_strings ss =
      let open Blake2.Blake2b in
      let b = init 28 in
      List.iter (fun s -> 
          update b (Bigstring.of_string s)) ss;
      let Hash bs = final b in
      Bigstring.to_string bs
  end

  let to_hex s =
    (* XXX not really fast I am afraid *)
    let buf = Buffer.create (String.length s * 2) in
    String.iter (fun c ->
        Buffer.add_string buf @@ Printf.sprintf "%02x" @@ Char.code c) s;
    Buffer.contents buf

  let _test =
    assert (to_hex @@ Blake2B_28.of_string "Replacing SHA1 with the more secure function"
            = "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578");
    assert (to_hex @@ Blake2B_28.of_strings ["Replacing SHA1 with the "; "more secure function" ]
            = "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578")
  (* obtained by
  #!/usr/local/bin/python3

  from hashlib import blake2b

  h = blake2b(digest_size=28)
  h.update(b'Replacing SHA1 with the more secure function')
  print (h.hexdigest())
  *)

    let hash_list xs = Blake2B_28.of_strings xs

    type t = string

    let to_string x = x
    let of_string x = assert (String.length x = 56); x

    let set_last_bit0 s =
      let len = String.length s in
      if len <> 28 then failwithf "set_last_bit0: len=%d <> 28" len; 
      let bs = Bytes.of_string s in
      Bytes.unsafe_set bs 27 
      @@ Char.chr @@ Char.code (Bytes.unsafe_get bs 27) land 0xfe;
      Bytes.to_string bs

    (* the hash of a leaf node with value v is taken as `H(0x00 || v)`, forcing the last
      bit to 0, and followed by a 223 0's and a 1.

       |<-      H(0x00 || v)        ->|
       |                            |0|0...........................01|

    *)
    let of_leaf v = 
      of_string
        (set_last_bit0 (hash_list [ "\000"; Value.to_string v])
         ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001")

    (* XXX correct? *)
    let of_empty_bud = of_string (String.make 56 '\000')

    (* the hash of a bud node is the hash of its child *)
    let of_bud = function
      | None -> of_empty_bud
      | Some h -> h

    (*
       |<-     H(0x01 || l || h)    ->|
       |                            |0|0...........................01|
    *)

    let of_internal_node l r =
      of_string 
      (set_last_bit0 (hash_list [ "\001"; l; r ])
       ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001")

    let segment_of_hash h =
      let sides_of_char c = 
        let c = Char.code c in
        let f x = if c land x = 0 then Path.Left else Path.Right in
        [f 128; f 64; f 32; f 16; f 8; f 4; f 2; f 1]
      in
      let rec f = function
        | 28 -> []
        | pos -> sides_of_char (String.unsafe_get h pos) @ f (pos+1)
      in
      let rec clean = function
        | [] -> assert false
        | Path.Right :: seg -> seg
        | Path.Left :: seg -> clean seg
      in
      Path.of_side_list @@ clean @@ f 0

    let hash_of_segment seg =
      let seg = (seg : Path.segment :> Path.side list) in
      let len = List.length seg in
      if len > 223 then failwith "segment is too long";
      let head_zero_bits = 224 - len - 1 in
      let head_zero_bytes = head_zero_bits / 8 in
      let bytes = Bytes.make 28 '\000' in
      let byte_pos = head_zero_bytes in
      let bit_pos = head_zero_bits mod 8 in
      let make_byte = function
        | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: seg ->
            let bit = function
              | Path.Left -> 0
              | Path.Right -> 1
            in
            let byte = 
              (bit x1) lsl 7
              + (bit x2) lsl 6
              + (bit x3) lsl 5
              + (bit x4) lsl 4
              + (bit x5) lsl 3
              + (bit x6) lsl 2
              + (bit x7) lsl 1
              + (bit x8) * 1
            in
            (Char.chr byte, seg)
        | _ -> assert false
      in
      let rec fill_bytes byte_pos = function
        | [] -> 
            assert (byte_pos = 28);
            Bytes.to_string bytes
        | seg ->
            let (c, seg) = make_byte seg in
            Bytes.unsafe_set bytes byte_pos c;
            let byte_pos' = byte_pos + 1 in
            if byte_pos' > 28 then assert false; (* segment is too long *)
            fill_bytes byte_pos' seg
      in
      fill_bytes byte_pos (List.init bit_pos (fun _ -> Path.Left) @ Path.Right :: seg)

    (*
       |<-                       H_child                           ->|
       |            224bits           |0........01|<- segment bits ->|
    *)
    let of_extender seg h =
      of_string (String.sub h 0 28 ^ hash_of_segment seg)
  end

  module KeyValueStore : sig
    (** Key-value store for leaf data using reference counting
        for deletion. TODO provide persistence to disk. *)

    (** Type of a key-value store. *)
    type t

    (** New, empty table *)
    val make : unit -> t

    (** Inserts a key in the table, or update the reference
        count if the key is already present. *)
    val insert   : t -> Value.t -> Hash.t

    (** Gets a value from the table, returns None if the key
        is not present. *)
    val get_opt  : t -> Hash.t -> Value.t option

    (** Decrements the reference counter of a key in the table.
        Deletes the key if the counter falls to 0. *)
    val decr     : t -> Hash.t -> unit

  end = struct
    type t = (Hash.t, Value.t * int) Hashtbl.t
    let make () = Hashtbl.create 0

    let hash_hash _ = assert false

    let insert table value =
      let h = hash_hash @@ Value.to_string value in (* XXX correct? *)
      match Hashtbl.find_opt table h with
      | None -> Hashtbl.add table h (value, 1) ; h
      | Some (_, count) -> Hashtbl.replace table h (value, count+1) ; h
    let get_opt table h =
      match Hashtbl.find_opt table h with
      | None -> None
      | Some (v, _) -> Some v
    let decr table h =
      match Hashtbl.find_opt table h with
      | None -> Printf.printf "none\n" ; ()
      | Some (_, 1) -> Printf.printf "some 1\n" ; Hashtbl.remove table h
      | Some (v, count) -> Printf.printf "some %d\n" count ;
        Hashtbl.replace table h (v, count-1)
  end

  type error = string
  type value = Value.t
  type segment = Path.segment
  type hash = Hash.t

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
      leaf_table  : KeyValueStore.t ;
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
        leaf_table = KeyValueStore.make () ;
        roots_table = Hashtbl.create 1 ;
        fd = fd ;
      }

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
    val hashed : node -> bool

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
      | Extender (_, Disk (_, Is_Extender), _, _, _) -> Error "Extender: cannot have Disk with Maybe_Extender"
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

    let view_indexing_rule_invariant : view -> (unit, error) result = function
      | Bud (None, Indexed _, _, _) -> Ok ()
      | Bud (Some n, Indexed _, _, _) when indexed n -> Ok ()
      | Bud (_, (Left_Not_Indexed | Right_Not_Indexed), _, _) -> Error "Bud: invalid indexing_rule"
      | Bud (_, Not_Indexed, _, _) -> Ok ()
      | Leaf (_, Indexed _, _, _) -> Ok ()
      | Leaf (_, Not_Indexed, _, _) -> Ok ()
      | Leaf (_, (Left_Not_Indexed | Right_Not_Indexed), _, _) -> Error "Leaf: invalid indexing_rule"
      | Internal (l, r, Indexed _, _, _) when indexed l && indexed r -> Ok ()
      | Internal (l, _r, Left_Not_Indexed, _, _) when not @@ indexed l -> Ok ()
      | Internal (_l, r, Right_Not_Indexed, _, _) when not @@ indexed r -> Ok ()
      | Internal (_l, _r, Not_Indexed, _, _) -> Error "Internal: invalid indexing_rule"
      | Extender (_, n, Indexed _, _, _) when indexed n -> Ok ()
      | Extender (_, _, Not_Indexed, _, _) -> Ok ()
      | Extender (_, _, (Left_Not_Indexed | Right_Not_Indexed), _, _) -> Error "Bud: invalid indexing_rule"
      | Bud (_, Indexed _, _, _)  
      | Internal (_, _, Indexed _, _, _)
      | Extender (_, _, Indexed _, _, _)  -> Error "Invalid Indexed"
      | Internal (_, _, Left_Not_Indexed, _, _) -> Error "Internal: invalid Left_Not_Indexed"
      | Internal (_, _, Right_Not_Indexed, _, _) -> Error "Internal: invalid Right_Not_Indexed"

    let hashed = function
      | Disk _ -> true
      | View (Bud (_, _, Hashed _, _)) -> true
      | View (Leaf (_, _, Hashed _, _)) -> true
      | View (Internal (_, _, _, Hashed _, _)) -> true
      | View (Extender (_, _, _, Hashed _, _)) -> true
      | _ -> false

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

    let view_invariant : view -> (unit, error) result = fun v ->
      view_shape_invariant v >>= fun () ->
      view_indexing_rule_invariant v >>= fun () ->
      view_hashed_is_transitive_invariant v

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

    let trail_invariant t = 
      trail_shape_invariant t >>= fun () ->
      trail_modified_rule_invariant t

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
            View (_Extender (Path.(@) segment seg, n, Not_Indexed, Not_Hashed, Not_Indexed_Any))
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

  let unify_extenders prev_trail node context =
    match node with
    | Disk (_, Is_Extender) -> Error "unify_exenders: Disk is not allowed"
    | View (Extender (seg, n, _, _, _)) ->
        begin match prev_trail with
          | Extended (prev_trail', seg', _mr, _iih) ->
              Ok (_Cursor (prev_trail', NotHashed.extend (Path.(@) seg' seg) n, context))
          | _ -> Ok (_Cursor (prev_trail, node, context))
        end
    | _ -> Ok (_Cursor (prev_trail, node, context))

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

  let hash (Cursor (_trail, node, context)) =
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
      | Leaf (value, Not_Indexed, _, _) ->
          let h = Hash.of_leaf value in
          (_Leaf (value, Not_Indexed, Hashed h, Not_Indexed_Any), h)

      | Bud (Some underneath, Not_Indexed, _, _) ->
          let (node, h) = hash_aux underneath in
          (_Bud (Some node, Not_Indexed, Hashed h, Not_Indexed_Any), h)

      | Bud (None, Not_Indexed, _, _) ->
          (_Bud (None, Not_Indexed, Hashed Hash.of_empty_bud, Not_Indexed_Any), Hash.of_empty_bud)

      | Internal (left, right, Left_Not_Indexed, _, _) -> (
          let (left, hl) = hash_aux left and (right, hr) = hash_aux right in
          let h = Hash.of_internal_node hl hr in
          (_Internal (left, right, Left_Not_Indexed, Hashed h, Not_Indexed_Any), h))

      | Internal (left, right, Right_Not_Indexed, _, _) -> (
          let (left, hl) = hash_aux left and (right, hr) = hash_aux right in
          let h = Hash.of_internal_node hl hr in
          (_Internal (left, right, Right_Not_Indexed, Hashed h, Not_Indexed_Any), h))

      | Extender (segment, underneath, Not_Indexed, _, _)  ->
          let (underneath, h) = hash_aux underneath in
          let h = Hash.of_extender segment h in
          (_Extender (segment, underneath, Not_Indexed, Hashed h, Not_Indexed_Any), h)
      | Leaf (_, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _)|
        Bud (None, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _)|
        Bud (Some _, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _)|
        Internal (_, _, (Not_Indexed|Indexed _), Not_Hashed, _)|
        Extender
          (_, _, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _) -> assert false

    in 
    let (_node, h) =  hash_aux node in 
    h
  (* TODO: return a new cursor instead and offer a way to extract a hash from a cursor *)

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
      | Leaf (value, Not_Indexed, _, _) ->
          let h = Hash.of_leaf value in
          (_Leaf (value, Not_Indexed, Hashed h, Not_Indexed_Any), h)

      | Bud (Some underneath, Not_Indexed, _, _) ->
          let (node, h) = hash_aux underneath in
          (_Bud (Some node, Not_Indexed, Hashed h, Not_Indexed_Any), h)

      | Bud (None, Not_Indexed, _, _) ->
          (_Bud (None, Not_Indexed, Hashed Hash.of_empty_bud, Not_Indexed_Any), Hash.of_empty_bud)

      | Internal (left, right, Left_Not_Indexed, _, _) -> (
          let (left, hl) = hash_aux left and (right, hr) = hash_aux right in
          let h = Hash.of_internal_node hl hr in
          (_Internal (left, right, Left_Not_Indexed, Hashed h, Not_Indexed_Any), h))

      | Internal (left, right, Right_Not_Indexed, _, _) -> (
          let (left, hl) = hash_aux left and (right, hr) = hash_aux right in
          let h = Hash.of_internal_node hl hr in
          (_Internal (left, right, Right_Not_Indexed, Hashed h, Not_Indexed_Any), h))

      | Extender (segment, underneath, Not_Indexed, _, _)  ->
          let (underneath, h) = hash_aux underneath in
          let h = Hash.of_extender segment h in
          (_Extender (segment, underneath, Not_Indexed, Hashed h, Not_Indexed_Any), h)
      | Leaf (_, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _)|
        Bud (None, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _)|
        Bud (Some _, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _)|
        Internal (_, _, (Not_Indexed|Indexed _), Not_Hashed, _)|
        Extender
          (_, _, (Left_Not_Indexed|Right_Not_Indexed|Indexed _), Not_Hashed, _) -> assert false

    in 
    let (node, h) =  hash_aux node in
    (_Cursor (trail, node, context), h)

  let commit _ = failwith "not implemented"
  let snapshot _ _ _ = failwith "not implemented"
  let update _ _ _ = failwith "not implemented"
  let gc ~src:_ _ ~dest:_ = failwith "not implemented"

  let open_context ~filename:_ = failwith "not implemented"
  let root _ _ = failwith "not implemented"
