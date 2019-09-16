(** Cursor (zipper) based tree operations *)

open Result
open Node

(* Trail and cursor *)

type modified =
  | Modified
  | Unmodified of indexed * hashed

type trail =
  | Top
  | Left of (* we took the left branch of an internal node *)
      trail
      * node
      * modified

  | Right of (* we took the right branch of an internal node *)
      node
      * trail
      * modified

  | Budded of
      trail
      * modified

  | Extended of
      trail
      * Segment.t
      * modified
  (* not the use of the "extender" and "not extender" type to enforce
     that two extenders cannot follow each other *)

let trail_shape_invariant = function
  | Extended (Extended _, _, _) -> Error "Extended: cannot have Extended"
  | Extended (_, seg, _) when Segment.is_empty seg -> Error "Extended: invalid empty segment"
  | _ -> Ok ()

let trail_modified_invariant = function
  | Top -> Ok ()
  | Left (_, n, Unmodified (ir, hit)) -> 
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> Error "Left: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Left: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Left (_, _, Modified) -> Ok ()
  | Right (n, _, Unmodified (ir, hit)) ->
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> Error "Right: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Right: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Right (_, _, Modified) -> Ok ()
  | Budded (_, Unmodified (ir, _hit)) ->
      begin match ir with
        | Indexed _ | Not_Indexed -> Ok ()
      end
  | Budded (_, Modified) -> Ok () 
  | Extended (_, _, Unmodified (ir, _hit)) ->
      begin match ir with
        | Indexed _ | Not_Indexed -> Ok ()
      end
  | Extended (_, _, Modified) -> Ok () 

let trail_index_and_hash_invariant = function
  | Top -> Ok ()
  | Left (_, _, Unmodified (Indexed _, Not_Hashed))
  | Right (_, _, Unmodified (Indexed _, Not_Hashed))
  | Budded (_, Unmodified (Indexed _, Not_Hashed))
  | Extended (_, _, Unmodified (Indexed _, Not_Hashed)) -> Error "Trail: Indexed with Not_Hashed"
  | _ -> Ok ()

let trail_invariant t = 
  trail_shape_invariant t >>= fun () ->
  trail_modified_invariant t >>= fun () ->
  trail_index_and_hash_invariant t

let check_trail t = 
  match trail_invariant t with
  | Ok _ -> t
  | Error s -> failwith s

let _Top = Top
let _Left (t, n, mr)     = check_trail @@ Left (t, n, mr)
let _Right (n, t, mr)    = check_trail @@ Right (n, t, mr)
let _Budded (t, mr)      = check_trail @@ Budded (t, mr)
let _Extended (t, s, mr) = check_trail @@ Extended (t, s, mr)

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

type t = cursor

let cursor_invariant (Cursor (trail, n, c)) =
  match trail with
  | Top -> 
      begin match view c n with
        | Bud _ -> Ok ()
        | _ -> Error "Cursor: Top has no Bud"
      end
  | Left (_, n', Unmodified (ir, hit)) -> 
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n && indexed n' -> Ok ()
        | Indexed _ -> Error "Cursor: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Cursor: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Left (_, _, Modified) -> Ok ()
  | Right (n', _, Unmodified (ir, hit)) ->
      begin match ir with
        | Not_Indexed -> Ok ()
        | Indexed _ when indexed n && indexed n' -> Ok ()
        | Indexed _ -> Error "Cursor: invalid Indexed"
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Cursor: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Right (_, _, Modified) -> Ok ()
  | Budded (_, Unmodified (ir, _hit)) ->
      begin match ir with
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> Error "Budded: invalid Indexed"
        | Not_Indexed -> Ok ()
      end
  | Budded (_, Modified) -> Ok ()
  | Extended (_, _, Unmodified (ir, hit)) ->
      begin match ir with
        | Indexed _ when indexed n -> Ok ()
        | Indexed _ -> Error "Extended: invalid Indexed"
        | Not_Indexed -> Ok ()
      end >>= fun () ->
      begin match hit with
        | Hashed _ when hashed n -> Ok ()
        | Hashed _ -> Error "Extended: invalid Hashed"
        | Not_Hashed -> Ok ()
      end
  | Extended (_, _, Modified) -> Ok ()

let check_cursor c = 
  match cursor_invariant c with
  | Ok _ -> c
  | Error s -> failwith s

let _Cursor (t, n, c) = 
  check_cursor @@ Cursor (t, n, c)

let segs_of_trail trail =
  let rec aux (xs, xss) = function
    | Top -> xs :: xss
    | Budded (tr, _) -> aux (Segment.empty, xs::xss) tr
    | Left (tr, _, _) -> aux (Segment.(cons Left xs), xss) tr
    | Right (_, tr, _) -> aux (Segment.(cons Right xs), xss) tr
    | Extended (tr, seg, _) -> aux (Segment.append seg xs, xss) tr
  in
  aux (Segment.empty, []) trail

let segs_of_cursor (Cursor (trail, _, _)) = segs_of_trail trail

let local_seg_of_trail trail =
  let rec aux xs = function
    | Top -> xs
    | Budded (_, _) -> xs
    | Left (tr, _, _) -> aux (Segment.cons Left xs) tr
    | Right (_, tr, _) -> aux (Segment.cons Right xs) tr
    | Extended (tr, seg, _) -> aux (Segment.append seg xs) tr
  in
  aux Segment.empty trail

let local_seg_of_cursor (Cursor (trail, _, _)) = local_seg_of_trail trail

let dot_of_cursor_ref = ref (fun _ -> assert false)
    
let attach trail node context =
  (* Attaches a node to a trail even if the indexing type and hashing type is 
     incompatible with the trail by tagging the modification. Extender types 
     still have to match. *)
  match trail with
  | Top -> _Cursor (_Top, node, context)
  | Left (prev_trail, right, _) ->
      _Cursor (_Left (prev_trail, right, Modified), node, context)
  | Right (left, prev_trail, _) ->
      _Cursor (_Right (left, prev_trail, Modified), node, context)
  | Budded (prev_trail, _) ->
      _Cursor (_Budded (prev_trail, Modified), node, context)
  | Extended (prev_trail, segment, _) ->
      _Cursor (_Extended (prev_trail, segment, Modified), node, context)

let view_cursor (Cursor (trail, n, context)) =
  let v = view context n in
  (_Cursor (trail, View (view context n), context), v)

let go_below_bud (Cursor (trail, n, context)) =
  (* This function expects a cursor positionned on a bud and moves it one step below. *)
  match view context n with
  | Bud (None,  _, _) -> Ok None
  | Bud (Some below, indexed, hashed) ->
      Ok (Some (_Cursor (
          _Budded (trail, Unmodified (indexed, hashed)), below,  context)))
  | _ -> Error "Attempted to navigate below a bud, but got a different kind of node."

let go_side side (Cursor (trail, n, context)) =
  (* Move the cursor down left or down right in the tree, assuming we are on an internal node. *)
  match view context n with
  | Internal (l, r, indexed, hashed) ->
      Ok (match side with
          | Segment.Right ->
              _Cursor (_Right (l, trail,
                             Unmodified (indexed, hashed)),
                       r, context)
          | Segment.Left ->
              _Cursor (_Left (trail, r,
                            Unmodified (indexed, hashed)),
                       l, context))
  | Extender _ -> Error "Attempted to navigate right or left of an extender"
  | _ -> Error "Attempted to navigate right or left of a non internal node"

let go_down_extender (Cursor (trail, n, context)) =
  (* Move the cursor down the extender it points to. *)
  match view context n with
  | Extender (segment, below, indexed, hashed) ->
      Ok (_Cursor (_Extended (trail, segment,
                            Unmodified (indexed, hashed)),
                   below, context))
  | _ -> Error "Attempted to go down an extender but did not find an extender"

(* Go up 1 level of tree.  
   Note that this can be more than 1 levels in segments,
   because of the extenders
*)
let go_up (Cursor (trail, node, context))  = match trail with
  | Top -> Error "cannot go above top"
  | Left (prev_trail, right,
          Unmodified (indexed, hashed)) ->
      let new_node =
        View (_Internal (node, right, indexed, hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Right (left, prev_trail,
           Unmodified (indexed, hashed)) ->
      let new_node =
        View (_Internal (left, node, indexed, hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Budded (prev_trail,
            Unmodified (indexed, hashed)) ->
      let new_node =
        View (_Bud (Some node, indexed, hashed))
      in Ok (_Cursor (prev_trail, new_node, context))

  | Extended (prev_trail, segment,
              Unmodified (indexed, hashed)) ->
    let new_node =
      View (_Extender (segment, node, indexed, hashed))
    in Ok (_Cursor (prev_trail, new_node, context))

  (* Modified cases. *)

  | Left (prev_trail, right, Modified) ->
      let internal = new_internal node right in
      Ok (attach prev_trail internal context)

  | Right (left, prev_trail, Modified) ->
      let internal = new_internal left node in
      Ok (attach prev_trail internal context)

  | Budded (prev_trail, Modified) ->
      let bud = new_bud @@ Some node in
      Ok (attach prev_trail bud context)

  | Extended (prev_trail, segment, Modified) ->
      let extender = new_extend segment node in
      Ok (attach prev_trail extender context)

let rec go_top (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> Ok c
  | _ -> go_up c >>= go_top

let rec go_up_to_a_bud c = 
  let c, v = view_cursor c in
  match v with
  | Bud _ -> Ok c (* already at a bud *)
  | _ -> go_up c >>= go_up_to_a_bud
(* XXX We can check trail instead of view *)

let parent c = 
  let c, v = view_cursor c in
  match v with
  | Bud _ -> go_up c >>= go_up_to_a_bud
  | _ -> Error "parent: cursor must be at a bud"

let unify_extenders prev_trail node context = match node with
  | Disk (_, Is_Extender) -> Error "unify_exenders: Disk is not allowed"
  | View (Extender (seg, n, _, _)) ->
      begin match prev_trail with
        | Extended (prev_trail', seg', _mr) ->
            Ok (attach prev_trail' (new_extend (Segment.append seg' seg) n) context)
        | _ -> Ok (attach prev_trail node context)
      end
  | _ -> Ok (attach prev_trail node context)

let rec remove_up trail context = match trail with
  | Top -> Error "cannot remove top"
  | Budded (prev_trail, _) ->
      Ok (attach prev_trail (new_bud None) context)
  | Extended (prev_trail, _, _) -> remove_up prev_trail context
  (* for Left and Right, we may need to squash Extenders in prev_trail *)
  | Left (prev_trail, right, _) ->
      (*
               /
              /\
         --> *  r

         We must load r because r can be an extender!
      *)
      let right = View (view context right) in
      let n = new_extend Segment.(of_side_list [Right]) right in
      unify_extenders prev_trail n context
  | Right (left, prev_trail, _) ->
      (*
               /
              /\
             l  * <--

         We must load l because l can be an extender!
      *)
      let left = View (view context left) in
      let n = new_extend Segment.(of_side_list [Left]) left in
      unify_extenders prev_trail n context

(* Let [c] is a cursor which points an Extender, whose segment is [common_prefix @ remaining_extender].
   [diverge c (common_prefix, remaining_extender, remaining_segment)] diverges a segment of [c] in the middle
   and create a path to [common_prefix @ remaining_segnet]. 
   It returns the newly created trail.
*)
let diverge (Cursor (trail, extender, _context)) (common_prefix, remaining_extender, remaining_segment) =
  match extender with
  | View (Extender (_seg, n, _ir, _hit)) -> (* _seg = common_prefix @ remaining_extender *)
      begin match Segment.cut remaining_segment, Segment.cut remaining_extender with
        | None, _ -> Error "diverge: remaining_segment is empty"
        | _, None -> Error "diverge: remaining_extender is empty"
        | Some (side, seg), Some (side', seg') -> 
            (* go down along common_prefix *)
            assert (side <> side');
            let trail = 
              if Segment.is_empty common_prefix then trail 
              else _Extended (trail, common_prefix, Modified)
            in
            let n' = new_extend seg' n in
            match side with
            | Segment.Left -> 
                if Segment.is_empty seg then
                  Ok (_Left (trail, n', Modified))
                else
                  Ok (_Extended (_Left (trail, n', Modified), seg, Modified))
            | Segment.Right -> 
                if Segment.is_empty seg then
                  Ok (_Right (n', trail, Modified))
                else
                  Ok (_Extended (_Right (n', trail, Modified), seg, Modified))
      end
  | _ -> Error "diverge: not an Extender"

(* Follow the segment from the cursor. If the segment terminates 
  or diverges in the middle of an extender, it returns the common prefix
  information. 
*)
type access_result =
  | Empty_bud (* The bud is empty *)
  | Collide of cursor * view (* The segment was blocked by an existing leaf or bud *)
  | Middle_of_extender of cursor * Segment.t * Segment.t * Segment.t (* The segment ends or diverges at the middle of an Extender with the common prefix, the remaining extender, and the rest of segment *)
  | Reached of cursor * view (* just reached to a node *)

let access_gen cur seg =
  let access_gen_aux cur segment =
    (* returns the cursor found by following the segment from the given cursor *)
    let rec aux (Cursor (trail, n, context)) segment =
      let v = view context n in
      let cur = _Cursor (trail, View v, context) in
      match Segment.cut segment with
      | None -> Ok (Reached (cur, v))
      | Some (dir, segment_rest) ->
          match v with
          | Leaf _ | Bud _ ->  Ok (Collide (cur, v))
          | Internal (l, r, indexed, hashed) -> begin
              match dir with
              | Left ->
                let new_trail = _Left (trail, r, Unmodified (indexed, hashed)) in
                aux (_Cursor (new_trail, l, context)) segment_rest
              | Right ->
                let new_trail = _Right (l, trail, Unmodified (indexed, hashed)) in
                aux (_Cursor (new_trail, r, context)) segment_rest
            end
          | Extender (extender, node_below, indexed, hashed) ->
            let (shared, remaining_extender, remaining_segment) =
              Segment.common_prefix extender segment in
            if remaining_extender = Segment.empty then
              let new_trail =
                _Extended (trail, extender, Unmodified (indexed, hashed)) in
              aux (_Cursor (new_trail, node_below, context)) remaining_segment
            else
              Ok (Middle_of_extender (cur, shared, remaining_extender, remaining_segment))
    in
    aux cur segment
  in
  go_below_bud cur >>= function
  | None -> Ok Empty_bud 
  | Some cur -> access_gen_aux cur seg

let error_access = function
  | Empty_bud -> Error "Nothing beneath this bud"
  | Collide _ -> Error "Reached to a leaf or bud before finishing"
  | Middle_of_extender (_, _, _, seg) when Segment.is_empty seg -> 
      Error "Finished at the middle of an Extender"
  | Middle_of_extender (_, _, _, _) -> 
      Error "Diverged in the middle of an Extender"
  | Reached (_, Bud _) -> Error "Reached to a Bud"
  | Reached (_, Leaf _) -> Error "Reached to a Leaf"

  | Reached (_, Internal _) -> Error "Reached to an Internal"
  | Reached (_, Extender _) -> Error "Reached to an Extender"
  
let subtree cur seg =
   access_gen cur seg >>= function
   | Reached (cur, Bud _) -> Ok cur
   | res -> error_access res

let get cur seg = 
  access_gen cur seg >>= function
  | Reached (_, Leaf (v, _, _)) -> Ok v
  | res -> error_access res

let get' cur seg = 
  access_gen cur seg >>= function
  | Reached (_, Leaf (v, _, _)) -> Ok (`Value v)
  | Reached (c, Bud _) -> Ok (`Bud c)
  | res -> error_access res

let empty context =
  (* A bud with nothing underneath, i.e. an empty tree or an empty sub-tree. *)
  _Cursor (_Top, new_bud None, context)

let delete cur seg =
  access_gen cur seg >>= function
  | Reached (Cursor (trail, _, context), (Bud _ | Leaf _)) -> 
      remove_up trail context 
      >>= go_up_to_a_bud
  | res -> error_access res

let alter (Cursor (trail, _, context) as cur) segment alteration =
  access_gen cur segment >>= function
  | Empty_bud -> 
      alteration None >>= fun n' ->
      let n' = new_extend segment n' in
      let n' = new_bud (Some n') in
      Ok (attach trail n' context)
  | (Middle_of_extender (_, _, _, seg) as res) when Segment.is_empty seg -> error_access res
  | (Reached (c, _) | Middle_of_extender (c, _, _, _) as res) ->
      (* XXX cleanup required *)
      let segsopt = match res with
        | Reached _ -> None
        | Middle_of_extender (_c, shared, rest_extender, rest_segment) ->
            Some (shared, rest_extender, rest_segment)
        | _ -> assert false
      in
      begin match segsopt with
        | None -> 
            (* Should we view the node? *)
            let Cursor (trail, n, context) = c in
            let v = view context n in
            Ok (trail, Some v)
        | Some segs -> diverge c segs >>| fun trail -> (trail, None)
      end >>= fun (trail, nopt) ->
      alteration nopt >>= fun n -> 
      let c = attach trail n context in
      (* go_up is required since c may point to a new bud *)
      go_up c >>= go_up_to_a_bud
  | res -> error_access res

let update cur segment value =
  access_gen cur segment >>= function
  | Reached (Cursor (trail, _, context), Leaf _) -> 
      go_up_to_a_bud (attach trail (View (_Leaf (value, Not_Indexed, Not_Hashed))) context)
  | res -> error_access res

let upsert cur segment value =
  alter cur segment (fun x ->
     let y = Ok (new_leaf value) in 
     match x with
     | None -> y
     | Some (Leaf _) -> y
     | Some _ -> Error "a non Leaf node already presents for this path")

let insert cur segment value =
  alter cur segment (function
      | None -> Ok (View (_Leaf (value, Not_Indexed, Not_Hashed)))
      | Some _ -> Error "a node already presents for this path")

let create_subtree cur segment =
  alter cur segment (function
      | None -> Ok (new_bud None)
      | Some _ -> Error "a node already presents for this path")

let subtree_or_create cur segment =
  (* XXX inefficient.  create_subtree should have an option not to go back to the original position *)
  let cur = 
    match create_subtree cur segment with
    | Ok cur -> cur
    | Error _ -> cur
  in
  subtree cur segment

type where_from =
  | From_above of dir
  | From_below of dir

and dir =
  | Left
  | Right
  | Center

(* Traversal step.  By calling this function repeatedly
   against the result of the function, we can traverse all the nodes.
   Note that this loads all the nodes in the memory in the end.
*)
let traverse (log, c) =
  let c, v = view_cursor c in
  match v, log with
  (* if going up, things below must be forgotten *)
  | _, From_below _ :: From_below _ :: _ -> assert false

  | Leaf _, From_below _ :: _ -> assert false
  | Leaf _, [] -> None
  | Leaf _, From_above d :: log -> 
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  | Bud (None, _, _), From_below _ :: _ -> assert false
  | Bud (None, _, _), [] -> None
  | Bud (None, _, _), From_above d :: log -> 
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  | Bud (Some _, _, _), From_below _ :: [] -> None
  | Bud (Some _, _, _), From_below _ :: From_above d :: log ->
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)
  | Bud (Some _, _, _), (From_above _ :: _ | []) ->
      let c = Utils.from_Some @@ from_Ok @@ go_below_bud c in
      Some (From_above Center :: log, c)

  | Internal _, From_below Center :: _ -> assert false
  | Internal _, (From_above _ :: _ | []) ->
      let c = from_Ok @@ go_side Segment.Left c in
      Some (From_above Left :: log, c)
  | Internal _, From_below Left :: log ->
      let c = from_Ok @@ go_side Segment.Right c in
      Some (From_above Right :: log, c)
  | Internal _, From_below Right :: [] -> None
  | Internal _, From_below Right :: From_above d :: log ->
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

  | Extender (_seg, _, _, _), (From_above _ :: _ | []) ->
      let c = from_Ok @@ go_down_extender c in
      Some (From_above Center :: log, c)
  | Extender (_, _, _, _), From_below _ :: [] -> None
  | Extender (_seg, _, _, _), From_below _ :: From_above d :: log ->
      let c = from_Ok @@ go_up c in
      Some (From_below d :: log, c)

(* Force the traversal go up and prevent it from going down from the node.
   This function only changes the direction.  It does not traverse.
*)
let force_traverse_up (log, c) =
  let c, v = view_cursor c in
  match v, log with
  (* we finish in the next traversal *) 
  | _, [] -> (log, c)

  (* the next traversal is go up *)
  | (Leaf _ | Bud (None, _, _)), log -> (log, c)
  | Bud (Some _, _, _), From_below Center :: _ -> (log, c)
  | Internal _, From_below Right :: _          -> (log, c)
  | Extender _, From_below Center :: _         -> (log, c)

  (* pretend it comes from the below *)
  | Bud (Some _, _, _), From_above _ :: _ ->
      (From_below Center :: log, c)
  | Internal _, From_above _ :: _ ->
      (* pretend it comes from the below-right *)
      (From_below Right :: log, c)
  | Internal _, From_below Left :: log ->
      (* pretend it comes from the below-right *)
      (From_below Right :: log, c)
  | Extender _, From_above _ :: _ ->
      (From_below Center :: log, c)

  (* impossible *)
  | Bud (Some _, _, _), From_below (Left | Right) :: _ -> assert false
  | Internal _, From_below Center :: _ -> assert false
  | Extender _, From_below (Left | Right) :: _ -> assert false

(* force_traverse_up + traverse *)
let traverse_up (log, c) =
  traverse @@ force_traverse_up (log,c)

let rec folder (log, c) =
  (* traverse anyway *)
  match traverse (log, c) with
  | None -> None
  | Some (log, c) ->
      let c, v = view_cursor c in
      match v, log with
      | Bud _, From_above _ :: _ -> 
          Some (force_traverse_up (log, c))
      | Leaf _, _ -> Some (log, c)
      | _ -> folder (log, c)

let fold ~init c f =
  go_below_bud c >>= function
  | None -> Ok (Ok init)
  | Some c ->
      let rec aux acc log c =
        let c, v = view_cursor c in
        match v, log with
        | Leaf _, From_above _ :: _ -> 
            let res = f acc c in
            begin match res with
              | Error e -> Error e
              | Ok acc -> 
                  match traverse (log, c) with
                  | None -> Ok acc
                  | Some (log, c) -> aux acc log c
            end
        | Bud _, From_above _ :: _ ->
            let res = f acc c in
            begin match res with
              | Error e -> Error e
              | Ok acc ->
                  let (log, c) = force_traverse_up (log, c) in
                  match traverse (log, c) with
                  | None -> Ok acc
                  | Some (log, c) -> aux acc log c
            end
        | _ -> 
            begin match traverse (log, c) with
              | None -> Ok acc
              | Some (log, c) -> aux acc log c
            end
      in
      Ok (aux init [] c)

let stat (Cursor (_,_,{ stat ; _ })) = stat

