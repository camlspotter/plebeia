open Result

type hashed =
  | Hashed of Hash.t
  | Not_Hashed
  (** Type used to prove that if a node is hashed then so are its children.
      The type also provides the hash as a witness.*)

type indexed =
  | Indexed of Index.t
  | Left_Not_Indexed (* Right may not be indexed either *)
  | Right_Not_Indexed (* Left may not be indexed either *)
  | Not_Indexed
  (** This rule expresses the following invariant : if a node is indexed, then
      its children are necessarily indexed. Less trivially, if an internal node is not
      indexed then at least one of its children is not yet indexed. The reason
      is that we never construct new nodes that just point to only existing nodes. 
      This property guarantees that when we write internal nodes on
      disk, at least one of the child can be written adjacent to its parent. *)

type extender_witness =
  | Maybe_Extender
  | Not_Extender  
  | Is_Extender   

type node =
  | Disk of Index.t * extender_witness
  (* Represents a node stored on the disk at a given index, the node hasn't
     been loaded yet. Although it's considered hashed for simplicity's sake,
     reading the hash requires a disk access and is expensive. *)

  | View of view
  (* A view node is the in-memory structure manipulated by programs in order
     to compute edits to the context. New view nodes can be commited to disk
     once the computations are done. *)

and view =
  | Internal of node * node
               * indexed
               * hashed

  (* An internal node , left and right children and an internal path segment
     to represent part of the path followed by the key in the tree. *)

  | Bud of node option
          * indexed
          * hashed
  (* Buds represent the end of a segment and the beginning of a new tree. They
     are used whenever there is a natural hierarchical separation in the key
     or, in general, when one wants to be able to grab sub-trees. For instance
     the big_map storage of a contract in Tezos would start from a bud. *)

  | Leaf of Value.t
          * indexed
          * hashed
  (* Leaf of a tree, the end of a path, contains or points to a value.
     The current implementation is a bit hackish and leaves are written
     on *two* cells, not one. This is important to keep in mind when
     committing the tree to disk.
  *)

  | Extender of Segment.t
                * node
                * indexed
                * hashed
  (* Extender node, contains a path to the next node. Represents implicitely
     a collection of internal nodes where one child is Null. *)

(* A trail represents the content of the memory stack when recursively exploring a tree.
   Constructing these trails from closure would be easier, but it would make it harder
   to port the code to C. The type parameters of the trail keep track of the type of each
   element on the "stack" using a product type. *)

let view_shape_invariant : view -> (unit, Error.t) Result.t = function
  | Bud (None, _, _) -> Ok ()
  | Bud (Some (Disk _), _, _) -> Error "Bud: cannot have Disk" (* or, we must load the Disk and check *)
  | Bud (Some (View (Bud _)), _, _) -> Error "Bud: cannot have Bud"
  | Bud (Some (View (Leaf _)), _, _) -> Error "Bud: cannot have Leaf"
  | Bud (Some (View (Internal _)), _, _) -> Ok ()
  | Bud (Some (View (Extender _)), _, _) -> Ok ()
  | Extender (seg, _, _, _) when Segment.is_empty seg -> Error "Extender: cannot have empty segment"
  | Extender (_, Disk (_, Not_Extender), _, _) -> Ok ()
  | Extender (_, Disk (_, Is_Extender), _, _) -> Error "Extender: cannot have Disk with Is_Extender"
  | Extender (_, Disk (_, Maybe_Extender), _, _) -> Error "Extender: cannot have Disk with Maybe_Extender"
  | Extender (_, View (Extender _), _, _) -> Error "Extender: cannot have Extender"
  | Extender (_, View _, _, _) -> Ok ()
  | Leaf _ -> Ok ()
  | Internal _ -> Ok ()

let indexed = function
  | Disk _ -> true
  | View (Bud (_, Indexed _, _)) -> true
  | View (Leaf (_, Indexed _, _)) -> true
  | View (Internal (_, _, Indexed _, _)) -> true
  | View (Extender (_, _, Indexed _, _)) -> true
  | View (Bud _ | Leaf _ | Internal _ | Extender _) -> false

let index = function
  | Disk (i,_) -> Some i
  | View (Bud (_, Indexed i, _)) -> Some i
  | View (Leaf (_, Indexed i, _)) -> Some i
  | View (Internal (_, _, Indexed i, _)) -> Some i
  | View (Extender (_, _, Indexed i, _)) -> Some i
  | View (Bud _ | Leaf _ | Internal _ | Extender _) -> None

let view_indexed_invariant : view -> (unit, Error.t) Result.t = function
  | Bud (None, Indexed _, _) -> Ok ()
  | Bud (Some n, Indexed _, _) when indexed n -> Ok ()
  | Bud (_, (Left_Not_Indexed | Right_Not_Indexed), _) -> Error "Bud: invalid indexed"
  | Bud (_, Not_Indexed, _) -> Ok ()
  | Leaf (_, Indexed _, _) -> Ok ()
  | Leaf (_, Not_Indexed, _) -> Ok ()
  | Leaf (_, (Left_Not_Indexed | Right_Not_Indexed), _) -> Error "Leaf: invalid indexed"
  | Internal (l, r, Indexed _i, _) ->
      begin match index l, index r with
        | None, _ -> Error "Internal: invalid Indexed"
        | _, None -> Error "Internal: invalid Indeced"
(*
        | Some li, Some ri -> 
            if Index.(i - li = one || i - ri = one) then Ok ()
            else Error "Internal: invalid indices"
*)
        | _ -> Ok () (* we now use fat internals *)
      end
  | Internal (l, _r, Left_Not_Indexed, _) when not @@ indexed l -> Ok ()
  | Internal (l, r, Left_Not_Indexed, _) when indexed l && indexed r -> 
      (* XXX workaround for node sharing *)
      Ok ()
  | Internal (_l, r, Right_Not_Indexed, _) when not @@ indexed r -> Ok ()
  | Internal (_l, _r, Not_Indexed, _) -> Error "Internal: invalid indexed"
  | Extender (_, n, Indexed _, _) when indexed n -> Ok ()
  | Extender (_, _, Not_Indexed, _) -> Ok ()
  | Extender (_, _, (Left_Not_Indexed | Right_Not_Indexed), _) -> Error "Bud: invalid indexed"
  | Bud (_, Indexed _, _)  
  | Extender (_, _, Indexed _, _)  -> Error "Invalid Indexed"
  | Internal (_, _, Left_Not_Indexed, _) -> Error "Internal: invalid Left_Not_Indexed"
  | Internal (_, _, Right_Not_Indexed, _) -> Error "Internal: invalid Right_Not_Indexed"

let hashed = function
  | Disk _ -> true
  | View (Bud (_, _, Hashed _)) -> true
  | View (Bud (_, _, Not_Hashed)) -> false
  | View (Leaf (_, _, Hashed _)) -> true
  | View (Leaf (_, _, Not_Hashed)) -> false
  | View (Internal (_, _, _, Hashed _)) -> true
  | View (Internal (_, _, _, Not_Hashed)) -> false
  | View (Extender (_, _, _, Hashed _)) -> true
  | View (Extender (_, _, _, Not_Hashed)) -> false

let hash_of_view = function
  | (Bud (_, _, Hashed h)) -> Some h
  | (Bud (_, _, Not_Hashed)) -> None
  | (Leaf (_, _, Hashed h)) -> Some h
  | (Leaf (_, _, Not_Hashed)) -> None
  | (Internal (_, _, _, Hashed h)) -> Some h
  | (Internal (_, _, _, Not_Hashed)) -> None
  | (Extender (_, _, _, Hashed h)) -> Some h
  | (Extender (_, _, _, Not_Hashed)) -> None

let view_hashed_invariant : view -> (unit, Error.t) Result.t = function
  | Leaf _ -> Ok ()
  | Bud (None, _, _) -> Ok ()
  | Bud (_, _, Not_Hashed) -> Ok ()
  | Bud (Some n, _, Hashed _) when hashed n -> Ok ()
  | Internal (l, r, _, Hashed _) when hashed l && hashed r -> Ok ()
  | Internal (_, _, _, Not_Hashed) -> Ok ()
  | Extender (_, n, _, Hashed _) when hashed n -> Ok ()
  | Extender (_, _, _, Not_Hashed) -> Ok ()
  | _ -> Error "Invalid Hashed"

let view_index_and_hash_invariant : view -> (unit, Error.t) Result.t = function
  | Bud (_, Indexed _, Not_Hashed)
  | Leaf (_, Indexed _, Not_Hashed)
  | Internal (_, _, Indexed _, Not_Hashed)
  | Extender (_, _, Indexed _, Not_Hashed) -> Error "View: Indexed with Not_Hashed"
  | _ -> Ok ()

let view_invariant : view -> (unit, Error.t) Result.t = fun v ->
  view_shape_invariant v >>= fun () ->
  view_indexed_invariant v >>= fun () ->
  view_hashed_invariant v >>= fun () ->
  view_index_and_hash_invariant v

let check_view v = 
  match view_invariant v with
  | Ok _ -> v
  | Error s -> failwith s

let _Internal (n1, n2, ir, hit) =
  check_view @@ Internal (n1, n2, ir, hit)

let _Bud (nopt, ir, hit) =
  check_view @@ Bud (nopt, ir, hit)

let _Leaf (v, ir, hit) =
  check_view @@ Leaf (v, ir, hit)

let _Extender (p, n, ir, hit) =
  check_view @@ Extender (p, n, ir, hit)

let new_leaf v = View (_Leaf (v, Not_Indexed, Not_Hashed))

let new_extend : Segment.segment -> node -> node = fun segment node ->
  if segment = Segment.empty then node
  else 
    match node with
    | View (Extender (seg, n, _, _)) ->
        View (_Extender (Segment.concat segment seg, n, Not_Indexed, Not_Hashed))
    | _ ->
        (* XXXX If the subnode is Disk, we have to make sure merging *)
        View (_Extender (segment, node, Not_Indexed, Not_Hashed))

let new_bud no = View (_Bud (no, Not_Indexed, Not_Hashed))

let new_internal n1 n2 i = 
  View (_Internal (n1, n2, i, Not_Hashed))



let load_node_ref = ref (fun _ _ _ -> assert false)

let load_node context index ewit = !load_node_ref context index ewit

let may_forget = function
  | Disk _ as n -> Some n
  | View (Internal (_, _, Indexed i, _)) -> Some (Disk (i, Not_Extender))
  | View (Bud (_, Indexed i, _)) -> Some (Disk (i, Not_Extender))
  | View (Leaf (_, Indexed i, _)) -> Some (Disk (i, Not_Extender))
  | View (Extender (_, _, Indexed i, _)) -> Some (Disk (i, Is_Extender))
  | _ -> None

let view c = function
  | Disk (i, wit) -> load_node c i wit
  | View v -> v
