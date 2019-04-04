open Types
open Error

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

let path_of_trail trail =
  let rec aux (xs, xss) = function
    | Top -> xs :: xss
    | Budded (tr, _, _) -> aux ([], xs::xss) tr
    | Left (tr, _, _, _) -> aux (Path.Left :: xs, xss) tr
    | Right (_, tr, _, _) -> aux (Path.Right :: xs, xss) tr
    | Extended (tr, seg, _, _) -> aux ((seg :> Path.side list) @ xs, xss) tr
  in
  aux ([], []) trail