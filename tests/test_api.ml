open Plebeia.Plebeia_impl
open Error
open Test_utils
open Cursor
open Deep

module RS = Random.State

module Debug = Plebeia.Debug

module Dumb = Dumb

let dump_cursor c =
  let Cursor (_, n, context) = c in
  to_file "plebeia.dot" @@ Debug.dot_of_cursor c;
  to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n
  
let () = 
  test_with_cursor @@ fun c ->
  save_to_dot "test_1.dot" c;
  let c = ok_or_fail @@ upsert c (path "LR") (value "fooLR") in
  save_to_dot "test_2.dot" c;
  let c = ok_or_fail @@ upsert c (path "LL") (value "fooLL") in
  let c = from_Some @@ ok_or_fail @@ go_below_bud c in
  let c = ok_or_fail @@ go_down_extender c in
  let c = ok_or_fail @@ go_side Segment.Left c in
  let c = ok_or_fail @@ go_up c in
  let c = ok_or_fail @@ go_side Segment.Right c in
  let c = ok_or_fail @@ go_up c in
  let c = ok_or_fail @@ go_up c in
  let c = ok_or_fail @@ go_up c in
  ignore c

let () = test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ upsert c (path "LLL") (value "LLL") in
  let c = ok_or_fail @@ upsert c (path "RRR") (value "RRR") in 
  let c = ok_or_fail @@ upsert c (path "LLR") (value "LLR") in 
  let c = ok_or_fail @@ upsert c (path "RRL") (value "RRL") in 
  let c = ok_or_fail @@ upsert c (path "LRL") (value "LRL") in 
  let c = ok_or_fail @@ upsert c (path "RLR") (value "RLR") in 
  let c = ok_or_fail @@ upsert c (path "LRR") (value "LRR") in
  ignore c

let () = 
  test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ insert c (path "RRRL") (value "RRRL") in
  let c = ok_or_fail @@ insert c (path "RLLR") (value "RLLR") in
  let c = ok_or_fail @@ insert c (path "RRRR") (value "RRRR") in
  save_to_dot "debug3.dot" c;
  let v = ok_or_fail @@ get c (path "RRRR") in
  assert (v = value "RRRR")

let () = 
  (* subtree *)
  test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ create_subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ create_subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ create_subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ create_subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ create_subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let v = value "RRR/RRR/RRR/RRR/RRR/LLL" in
  let c = ok_or_fail @@ insert c (path "LLL") v in
  let c = ok_or_fail @@ go_top c in
  let c, _, _ = ok_or_fail @@ commit c in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let v' = ok_or_fail @@ get c (path "LLL") in
  assert (v = v')

let () = 
  test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ create_subtree c (path "LRRR") in
  let c = ok_or_fail @@ go_top c in
  let c = ok_or_fail @@ create_subtree c (path "LLLR") in
  let c = ok_or_fail @@ go_top c in
  let c, _, _ = ok_or_fail @@ commit c in
  let c = ok_or_fail @@ create_subtree c (path "RRRR") in
  let c = ok_or_fail @@ go_top c in
  let c = ok_or_fail @@ subtree c (path "LLLR") in
  let c = ok_or_fail @@ create_subtree c (path "LRRL") in (* once failed here due to a bug of alter *)
  ignore c

let () =
  test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ create_subtree c (path "RRR") in
  let c = ok_or_fail @@ go_top c in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  let c = ok_or_fail @@ insert c (path "RRL") (value "1") in
  let c = ok_or_fail @@ go_top c in
  let c, _, _ = ok_or_fail @@ commit c in
  let c = ok_or_fail @@ subtree c (path "RRR") in
  save_to_dot "debug.dot" c;
  let c = ok_or_fail @@ delete c (path "RRL") in (* Once failed here *)
  ignore c
    
let () = 
  ignore @@ from_Ok @@ test_with_cursor @@ fun c ->
  let c = from_Ok @@ insert c (path "RR") (value "RR") in
  let _ = 
    (* It succeeded by putting a Leaf at "RR" instead at "RRRR"...  Now fixed. *)
    from_Error @@ upsert c (path "RRRR") (value "RRRR") 
  in
  return ()

let test_path_of_trail c seg = 
  match access_gen c seg with
  | Ok (Reached (Cursor (trail, _, _), _v)) ->
      if List.flatten @@ path_of_trail trail <> seg then begin
        failwith
          (String.concat "/" (List.map (fun x -> Segment.of_side_list x |> Segment.to_string) (path_of_trail trail))
        ^ "  /= " ^ Segment.to_string seg)
      end
  | Ok (Middle_of_extender _) ->
      (* middle of extender *)
      dump_cursor c;
      assert false
  | Ok x -> 
      let e = match error_access x with
        | Ok _ -> assert false
        | Error e -> e
      in
      dump_cursor c;
      failwith e
        
  | Error e -> 
      (* no path ? *)
      dump_cursor c;
      failwith e

let validate context n =
  default (Debug.validate_node context n) (fun e -> 
      to_file "invalid.dot" @@ Debug.dot_of_node n;
      prerr_endline "Saved the current node to invalid.dot";
      failwith e)

(* Add random leafs and subdirs to plebeia and dumb trees,
   checking the consistency between them.  
   It returns, the final trees and a hashtbl of added leafs and subdirs.
   
   The input trees must be equivalent.
*)
let add_random st sz c dumb =
  let bindings = Hashtbl.create 101 in

  let random_segment st =
    let length = RS.int st 10 + 3 in
    random_segment ~length st 
  in

  let rec f c dumb i =
    if i = sz then (c, dumb)
    else 
      let seg = random_segment st in
      let c, dumb = 
        let s = Segment.to_string seg in
        let v = value (Segment.to_string seg) in

        (* get *)
        begin match get c seg, Dumb.get dumb seg with
          | Ok _, Ok _ -> ()
          | Error _, Error _ -> ()
          | _ -> assert false
        end;

        (* subtree *)
        begin match subtree c seg, Dumb.subtree dumb seg with
          | Ok _, Ok _ -> ()
          | Error _, Error _ -> ()
          | _ -> assert false
        end;

        (* removal (we do not accumulate) *)
        begin match 
            delete c seg,
            Dumb.delete dumb seg
          with
          | Ok _, Ok _ -> ()
          | Error _, Error _ -> ()
          | Ok _, Error e -> 
              Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
          | Error e, Ok _ -> 
              Format.eprintf "impl: %s (seg=%s)@." e s; 
              Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
              assert false
        end;

        let compare_trees dumb (Cursor (_, n, context) as c) =
          (* compare the entire tree *)
          if Dumb.get_node dumb <> Dumb.of_plebeia_node context n then begin
            to_file "dumb.dot" @@ Dumb.dot_of_cursor dumb;
            to_file "plebeia.dot" @@ Debug.dot_of_cursor c;
            to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
            begin match go_top c with
              | Error _ -> prerr_endline "no root dump"
              | Ok (Cursor (_, n, context)) ->
                  to_file "plebeia_dumb_root.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
            end;
            assert false
          end
        in

        match RS.int st 3 with
        | 0 -> begin
            (* insert *)
            match 
              insert c seg v,
              Dumb.insert dumb seg v
            with
            | Ok c, Ok dumb -> 
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                (* check the invariants of the node *)
                validate context n;

                (* record the insertion *)
                Hashtbl.replace bindings seg (`Value v);
                test_path_of_trail c seg;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ -> 
                Format.eprintf "impl: %s (seg=%s)@." e s; 
                Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
                assert false
          end
        | 1 -> begin
            (* upsert *)
            match 
              upsert c seg v,
              Dumb.upsert dumb seg v
            with
            | Ok c, Ok dumb -> 
                (* print_command (); *)
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                validate context n;
                Hashtbl.replace bindings seg (`Value v);
                test_path_of_trail c seg;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ -> 
                Format.eprintf "impl: %s (seg=%s)@." e s; 
                Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
                assert false
          end
        | 2 -> begin
            (* create_subtree *)
            match 
              create_subtree c seg,
              Dumb.create_subtree dumb seg
            with
            | Ok c, Ok dumb -> 
                (* print_command (); *)
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                validate context n;
                Hashtbl.replace bindings seg `Subtree;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ -> 
                Format.eprintf "impl: %s (seg=%s)@." e s; 
                Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
                assert false
          end
        | _ -> assert false
      in
      f c dumb (i+1)
  in
  let c, dumb = f c dumb 0 in
  to_file "random_insertions.dot" @@ Debug.dot_of_cursor c;
  Hashtbl.iter (fun seg x -> 
      match x with
      | `Value v -> assert (get c seg = Ok v)
      | `Subtree -> assert (match subtree c seg with Ok _ -> true | _ -> false)
    ) bindings;

  (* hash and commit *)
  let _c, _ = hash c in
  let c, _, _ = from_Ok @@ commit c in

  c, dumb, bindings

  let random_insertions st sz =
    test_with_cursor @@ fun c ->

    let dumb = Dumb.empty () in

    let c, dumb, bindings = add_random st sz c dumb in

    (* traversal: visit the leaf and bud and check all are covered *)
    let bindings' = Hashtbl.copy bindings in
    let rec loop (log, Cursor (trail, n, context) as pos) =
      begin match log, view context n with
      | ([] | From_above _ :: _), Leaf _ ->
          let s = match path_of_trail trail with [[]; s] -> s | _ -> assert false in
          (* Format.eprintf "value seg: %s@." @@ Segment.to_string s; *)
          begin match Hashtbl.find_opt bindings' s with
            | Some `Value _ -> Hashtbl.remove bindings' s
            | _ -> assert false
          end
      | ([] | From_above _ :: _), Bud _ ->
          begin match path_of_trail trail with 
            | [[]] -> ()
            | [[]; s] ->
                begin
                  (* Format.eprintf "subtree seg: %s@." @@ Segment.to_string s; *)
                  match Hashtbl.find_opt bindings' s with
                  | Some `Subtree -> Hashtbl.remove bindings' s
                  | _ -> assert false
                end
            | _ -> assert false
          end
      | _ -> ()
      end;
      match traverse pos with
      | None -> 
          assert (Hashtbl.fold (fun k v acc -> (k,v)::acc) bindings' [] = [])
      | Some pos -> loop pos
    in
    loop ([], c);

    (* deletions to the empty *)
    let bindings = shuffle st @@ Hashtbl.fold (fun k v st -> (k,v)::st) bindings [] in
    let Cursor (_, n, _), _ = 
      List.fold_left (fun (c, dumb) (seg, _) ->
          let Cursor (_, n, context) as c = match delete c seg with 
            | Ok c -> 
                let _c, _ = hash c in
                let c, _, _ = from_Ok @@ commit c in
                c
            | Error e -> 
                to_file "deletion.dot" @@ Debug.dot_of_cursor c;
                failwith e
          in
          let dumb = from_Ok @@ Dumb.delete dumb seg in
          assert (Dumb.get_node dumb = Dumb.of_plebeia_node context n);
          validate context n;
          (c, dumb)) (c, dumb) bindings
    in
    match n with
    | View (Bud (None, _, _)) -> ()
    | _ -> assert false

let () = 
  let st = RS.make_self_init () in
  for _ = 1 to 1000 do random_insertions st 100 done
