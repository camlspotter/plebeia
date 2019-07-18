open Plebeia.Impl
open Roots2
open Test_utils
open Stdint

let () = exit 0

module RS = Random.State
              
let random_hash st =
  Hash.of_string @@ random_string st 28
  
let random_index st =
  Uint32.of_int64 @@ RS.int64 st @@ Int64.(of_uint32 Uint32.max_int - 256L + 1L)

let st = Random.State.make_self_init ()

let () = 
  test_with_context 100000 @@ fun c ->
  let t = create c in
  let choose xs =
    let pos = RS.int st (List.length xs) in
    List.nth xs pos
  in
  let rec loop acc = function
    | 0 -> ()
    | n ->
        let v = Value.of_string @@ string_of_int n in
        let lh = Node_hash.of_leaf v in
        let h = Node_hash.shorten lh in
        let _, i, _ = Storage.write_leaf c v lh in
        let parent = match acc with
          | [] -> None
          | _ -> 
              if RS.int st 2 = 0 then None
              else Some (choose acc)
        in
        Roots2.add t ?parent h i;
        loop (i::acc) (n-1)
  in
  loop [] 100;
  let t' = create c in
  assert (List.sort compare (Hashtbl.fold (fun k v acc -> (k,v)::acc) t.tbl [])
          = List.sort compare (Hashtbl.fold (fun k v acc -> (k,v)::acc) t'.tbl []))

