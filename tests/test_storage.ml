(* node storage test *)
open Plebeia.Impl
open Test_utils
open Debug
open Node

module RS = Random.State

let parse_test context n =
  (* reload the node and compare *)
  try
    let n' = View (Node_storage.parse_cell context.Context.storage @@ from_Some @@ index n) in
    let n' = Node_storage.load_node_fully context n' in
    if n <> n' then begin
      prerr_endline @@ string_of_node n 2;
      prerr_endline @@ string_of_node n' 2;
      assert false
    end
  with
  | e ->
      prerr_endline @@ string_of_node n 2;
      raise e

let random_write_parse st =
  test_with_cursor @@ fun (Cursor (_, _, context)) ->

  match RS.int st 4 with
  | 0 (* leaf *) ->
      let size = RS.int st 64 in
      let v = Value.of_string @@ random_string st size in
      let n = new_leaf v in
      let n, _i, _h = Node_storage.commit_node context n in
      parse_test context n

  | 1 (* bud *) ->
      if RS.bool st then
        let n = new_bud None in
        let n, _, _ = Node_storage.commit_node context n in
        parse_test context n
      else
        let size = RS.int st 64 in
        let v = Value.of_string @@ random_string st size in
        let n = new_bud (Some (new_extend (path "L") (new_leaf v))) in
        let n, _, _ = Node_storage.commit_node context n in
        parse_test context n

  | 2 (* internal *) ->
      let right_referred = RS.bool st in
      let n1, _, _ =
        Node_storage.commit_node context @@
        let size = RS.int st 16 in
        new_leaf @@ Value.of_string @@ random_string st size
      in
      let n2 =
        let size = RS.int st 16 in
        new_leaf @@ Value.of_string @@ random_string st size
      in
      let n =
        if right_referred then
          new_internal n2 n1 Left_Not_Indexed
        else
          new_internal n1 n2 Right_Not_Indexed
      in
      let n, _, _ = Node_storage.commit_node context n in
      parse_test context n

  | 3 (* extender *) ->
      let seg = random_segment st in
      let n' =
        let size = RS.int st 16 in
        new_leaf @@ Value.of_string @@ random_string st size
      in
      let n = new_extend seg n' in
      let n, _, _ = Node_storage.commit_node context n in
      parse_test context n

  | _ -> assert false


let () =
  let st = Random.State.make_self_init () in
  for _ = 1 to 10000 do random_write_parse st done
