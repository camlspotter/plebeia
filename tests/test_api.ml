open Plebeia.Plebeia_impl
open Error
open Test_utils

module Debug = Plebeia.Debug

module Dumb = Dumb
  
let () =
  ignore @@ from_Ok @@ test_with_context @@ fun cursor ->
  upsert cursor (path_of_string "LR") (Value.of_string "fooLR") >>= fun cursor -> 
  upsert cursor (path_of_string "LL") (Value.of_string "fooLL") 
  >>= go_below_bud >>= go_down_extender >>= go_side Path.Left >>= go_up
  >>= go_side Path.Right >>= go_up >>= go_up >>= go_up

let () = ignore @@ from_Ok @@ test_with_context @@ fun c ->
  upsert c (path_of_string "LLL") (Value.of_string "LLL") >>= fun c -> 
  upsert c (path_of_string "RRR") (Value.of_string "RRR") >>= fun c -> 
  upsert c (path_of_string "LLR") (Value.of_string "LLR") >>= fun c -> 
  upsert c (path_of_string "RRL") (Value.of_string "RRL") >>= fun c -> 
  upsert c (path_of_string "LRL") (Value.of_string "LRL") >>= fun c -> 
  upsert c (path_of_string "RLR") (Value.of_string "RLR") >>= fun c -> 
  upsert c (path_of_string "LRR") (Value.of_string "LRR")

let () = 
  ignore @@ from_Ok @@ test_with_context @@ fun c ->
  let c = from_Ok @@ insert c (path_of_string "RRRL") (Value.of_string "RRRL") in
  let c = from_Ok @@ insert c (path_of_string "RLLR") (Value.of_string "RLLR") in
  let c = from_Ok @@ insert c (path_of_string "RRRR") (Value.of_string "RRRR") in
  let Cursor (_, n, _) = c in to_file "debug3.dot" (Debug.dot_of_node n);
  let v = match get c (path_of_string "RRRR") with
    | Error e -> failwith e
    | Ok v -> v
  in
  assert (v = Value.of_string "RRRR");
  return () (* once failed here due to a bug. Now fixed. *)

let () = 
  ignore @@ from_Ok @@ test_with_context @@ fun c ->
  let c = from_Ok @@ insert c (path_of_string "RR") (Value.of_string "RR") in
  let _ = 
    (* It succeeded by putting a Leaf at "RR" instead at "RRRR"...  Now fixed. *)
    from_Error @@ upsert c (path_of_string "RRRR") (Value.of_string "RRRR") 
  in
  return ()

let random_insertions st sz =
  let validate context n =
    default (Debug.validate_node context n) (fun e -> 
        to_file "invalid.dot" @@ Debug.dot_of_node n;
        prerr_endline "Saved the current node to invalid.dot";
        failwith e);
  in
  test_with_context @@ fun c ->
  let bindings = Hashtbl.create 101 in
  let rec f c dumb i =
    if i = sz then (c, dumb)
    else 
      let length = Random.State.int st 10 + 3 in
      let seg = random_segment ~length st in
      let c, dumb = 
        let s = Path.to_string seg in
        let v = Value.of_string (Path.to_string seg) in
(*
        let print_command () =
          Format.eprintf "insert c (path_of_string %S) (Value.of_string %S) >>= fun c ->@." s s
        in
*)
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

        match Random.State.int st 3 with
        | 0 -> begin
            (* insert *)
            match 
              insert c seg v,
              Dumb.insert dumb seg v
            with
            | Ok c, Ok dumb -> 
                (* print_command (); *)
                let Cursor (_, n, context) = c in
                if Dumb.get_root_node dumb <> Dumb.of_plebeia_node context n then begin
                  to_file "dumb.dot" @@ Dumb.dot_of_cursor dumb;
                  to_file "plebeia.dot" @@ from_Ok @@ Debug.dot_of_cursor c;
                  to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
                  assert false
                end;
                validate context n;
                Hashtbl.replace bindings seg (`Value v);
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ -> 
                Format.eprintf "impl: %s (seg=%s)@." e s; 
                Format.eprintf "%s@." @@ from_Ok @@ Debug.dot_of_cursor c;
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
                let Cursor (_, n, context) = c in
                if Dumb.get_root_node dumb <> Dumb.of_plebeia_node context n then begin
                  to_file "dumb.dot" @@ Dumb.dot_of_cursor dumb;
                  to_file "plebeia.dot" @@ from_Ok @@ Debug.dot_of_cursor c;
                  to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
                  assert false
                end;
                validate context n;
                Hashtbl.replace bindings seg (`Value v);
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ -> 
                Format.eprintf "impl: %s (seg=%s)@." e s; 
                Format.eprintf "%s@." @@ from_Ok @@ Debug.dot_of_cursor c;
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
                let Cursor (_, n, context) = c in
                if Dumb.get_root_node dumb <> Dumb.of_plebeia_node context n then begin
                  to_file "dumb.dot" @@ Dumb.dot_of_cursor dumb;
                  to_file "plebeia.dot" @@ from_Ok @@ Debug.dot_of_cursor c;
                  to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
                  assert false
                end;
                validate context n;
                Hashtbl.replace bindings seg `Subtree;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ -> 
                Format.eprintf "impl: %s (seg=%s)@." e s; 
                Format.eprintf "%s@." @@ from_Ok @@ Debug.dot_of_cursor c;
                assert false
          end
        | _ -> assert false
      in
      f c dumb (i+1)
  in
  let dumb = Dumb.empty () in
  let c, dumb = f c dumb 0 in
  to_file "random_insertions.dot" @@ from_Ok @@ Debug.dot_of_cursor c;
  Hashtbl.iter (fun seg x -> 
      match x with
      | `Value v -> assert (get c seg = Ok v)
      | `Subtree -> assert (match subtree c seg with Ok _ -> true | _ -> false)
    ) bindings;
  
  (* deletion *)
  let bindings = shuffle st @@ Hashtbl.fold (fun k v st -> (k,v)::st) bindings [] in
  let Cursor (_, n, _), _ = 
    List.fold_left (fun (c, dumb) (seg, _) ->
        let Cursor (_, n, context) as c = from_Ok @@ delete c seg in
        let dumb = from_Ok @@ Dumb.delete dumb seg in
        assert (Dumb.get_root_node dumb = Dumb.of_plebeia_node context n);
        validate context n;
        (c, dumb)) (c, dumb) bindings
  in
  match n with
  | View (Bud (None, _, _, _)) -> ()
  | _ -> assert false

let () = 
  let st = Random.State.make_self_init () in
  for _ = 1 to 1000 do random_insertions st 100 done

