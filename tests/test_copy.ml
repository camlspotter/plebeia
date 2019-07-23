open Plebeia.Impl
open Result
open Test_utils
open Cursor

module RS = Random.State

module Dumb = Dumb

let dump_cursor c =
  let Cursor (_, n, context) = c in
  to_file "plebeia.dot" @@ Debug.dot_of_cursor c;
  to_file "plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n
  
let () = test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ create_subtree c (path "LLL") in
  let c, () = ok_or_fail @@ Deep.deep ~go_up:true ~create_subtrees:true c [path "LLL"; path "RRR"] (fun cur seg -> upsert cur seg (value "LLL/RRR") >>| fun c -> (c, ())) in
  let Cursor (trail, _, _) = c in
  if trail <> _Top then begin
    Format.eprintf "deep strange return cursor: %s@." (String.concat "/" @@ List.map Segment.to_string @@ segs_of_trail trail);
    assert false
  end;
  let c = ok_or_fail @@ Deep.copy ~create_subtrees:true c [path "LLL"] [path "RRR"] in
  save_to_dot "copy.dot" c;
  let v = ok_or_fail @@ Deep.get c [path "RRR"; path "RRR"] in
  assert (v = (value "LLL/RRR"));
  (* try to create a loop *)
  let () = must_fail @@ Deep.copy ~create_subtrees:false c [] [path "LLR"] in
  
  ignore c
    
let () = test_with_cursor @@ fun c ->
  let path_key k = match Key.to_segments k with Ok [seg] -> seg | _ -> assert false in
  let c = ok_or_fail @@ Deep.insert c
      (List.map path_key ["data"; "rolls"; "owner"; "current"; "69"; "56"; "14405"])
      (value "x")
  in
  let c = ok_or_fail @@ Deep.copy ~create_subtrees:true c 
      (List.map path_key ["data"; "rolls"; "owner"; "current"])
      (List.map path_key ["data"; "rolls"; "owner"; "snapshot"; "0"; "0"])
  in
  save_to_dot "copy3.dot" c;
  let v = ok_or_fail @@ Deep.get c 
      (List.map path_key ["data"; "rolls"; "owner"; "snapshot"; "0"; "0"; "69"; "56"; "14405"])
  in
  assert (v = (value "x"));
  
  let c, i, _ = Cursor_storage.commit_cursor c in

  let Cursor (trail, node, context) = c in
  let node = match Node.may_forget node with
    | None -> assert false
    | Some n -> n
  in
  let c = _Cursor (trail, node , context) in
  let v = ok_or_fail @@ Deep.get c 
      (List.map path_key ["data"; "rolls"; "owner"; "snapshot"; "0"; "0"; "69"; "56"; "14405"])
  in
  assert (v = (value "x"));
  
  let c = _Cursor (trail, Disk (i, Not_Extender), context) in
  let v = ok_or_fail @@ Deep.get c 
      (List.map path_key ["data"; "rolls"; "owner"; "snapshot"; "0"; "0"; "69"; "56"; "14405"])
  in
  assert (v = (value "x"));
  

