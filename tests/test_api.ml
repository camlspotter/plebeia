open Plebeia
open Plebeia.Plebeia_impl
open Error
open Test_utils
open Cursor

module Debug = Plebeia.Debug

module Dumb = Dumb

let () = 
  test_with_cursor @@ fun c ->
  save_to_dot "test_1.dot" c;
  let c = ok_or_fail @@ upsert c (path "LR") (value "fooLR") in
  save_to_dot "test_2.dot" c;
  let c = ok_or_fail @@ upsert c (path "LL") (value "fooLL") in
  let c = from_Some @@ ok_or_fail @@ go_below_bud c in
  let c = ok_or_fail @@ go_down_extender c in
  let c = ok_or_fail @@ go_side Path.Left c in
  let c = ok_or_fail @@ go_up c in
  let c = ok_or_fail @@ go_side Path.Right c in
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
  ignore @@ from_Ok @@ test_with_cursor @@ fun c ->
  let c = from_Ok @@ insert c (path "RR") (value "RR") in
  let _ = 
    (* It succeeded by putting a Leaf at "RR" instead at "RRRR"...  Now fixed. *)
    from_Error @@ upsert c (path "RRRR") (value "RRRR") 
  in
  return ()

let dump_cursor c =
  let Cursor (_, n, context) = c in
  to_file "plebeia.dot" @@ Debug.dot_of_cursor c;
  to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n
  
let test_path_of_trail c seg = 
  let c' = from_Some @@ from_Ok @@ go_below_bud c in
  match access_gen c' seg with
  | Ok (Cursor (trail, _, _), None) ->
      if List.flatten @@ path_of_trail trail <> seg then begin
        failwith
          (String.concat "/" (List.map (fun x -> Path.of_side_list x |> Path.to_string) (path_of_trail trail))
        ^ "  /= " ^ Path.to_string seg)
      end
  | Ok (Cursor (_trail, _, _), Some _) -> 
      (* middle of extender *)
      dump_cursor c;
      assert false
  | Error e -> 
      (* no path ? *)
      dump_cursor c;
      failwith e

let validate context n =
  default (Debug.validate_node context n) (fun e -> 
      to_file "invalid.dot" @@ Debug.dot_of_node n;
      prerr_endline "Saved the current node to invalid.dot";
      failwith e)

let random_insertions st sz =
  test_with_cursor @@ fun c ->
  let bindings = Hashtbl.create 101 in

  let random_segment st =
    let length = Random.State.int st 10 + 3 in
    random_segment ~length st 
  in

  let rec f c dumb i =
    if i = sz then (c, dumb)
    else 
      let seg = random_segment st in
      let c, dumb = 
        let s = Path.to_string seg in
        let v = value (Path.to_string seg) in

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

        match Random.State.int st 3 with
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
  let dumb = Dumb.empty () in
  let c, dumb = f c dumb 0 in
  to_file "random_insertions.dot" @@ Debug.dot_of_cursor c;
  Hashtbl.iter (fun seg x -> 
      match x with
      | `Value v -> assert (get c seg = Ok v)
      | `Subtree -> assert (match subtree c seg with Ok _ -> true | _ -> false)
    ) bindings;

  (* hash and commit *)
  let _c, _ = hash c in
  let c, _ = from_Ok @@ commit c in

  (* deletions to the empty *)
  let bindings = shuffle st @@ Hashtbl.fold (fun k v st -> (k,v)::st) bindings [] in
  let Cursor (_, n, _), _ = 
    List.fold_left (fun (c, dumb) (seg, _) ->
        let Cursor (_, n, context) as c = match delete c seg with 
          | Ok c -> 
              let _c, _ = hash c in
              let c, _ = from_Ok @@ commit c in
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
  | View (Bud (None, _, _, _)) -> ()
  | _ -> assert false

let () = 
  let st = Random.State.make_self_init () in
  for _ = 1 to 1000 do random_insertions st 100 done
