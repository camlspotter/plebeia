open Plebeia
open Plebeia.Plebeia_impl

include Utils
    
let timed f = 
  let t1 = Unix.gettimeofday () in
  let res = Exn.catch f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

module RS = Random.State
              
let random_string st len = String.init len (fun _ -> Char.chr @@ RS.int st 256)
    
let random_segment ?length st =
  let open Path in
  let open Random.State in
  let length = 
    match length with
    | None -> int st 221 + 1 (* 1..222 *)
    | Some l -> l
  in
  assert (0 < length && length <= 222); (* XXX constant *)
  let rec f = function
    | 0 -> []
    | n -> (if bool st then Left else Right) :: f (n-1)
  in
  of_side_list @@ f length

let from_Some = function
  | Some x -> x
  | _ -> failwith "Not Some"

let to_file fn s =
  let oc = open_out fn in
  output_string oc s;
  close_out oc

let shuffle st xs =
  let a = Array.of_list xs in
  let size = Array.length a in
  for i = 0 to size - 2 do
    let pos = Random.State.int st (size - i - 1) + i in
    let x = Array.unsafe_get a pos in
    Array.unsafe_set a pos @@ Array.unsafe_get a i;
    Array.unsafe_set a i x
  done;
  Array.to_list a

let test_with_context length f =
  let context =
    let fn = Filename.temp_file "plebeia" "test" in
    Context.make ~shared:true ~length fn
  in
  Exn.protect (fun () -> f context) (fun () -> Context.free context)
  
let test_with_cursor f =
  let context =
    let fn = Filename.temp_file "plebeia" "test" in
    Context.make ~shared:true ~length:1000000 fn
  in
  let cursor = empty context in
  Exn.protect (fun () -> f cursor) (fun () -> Context.free context)
    
let path_of_string s = from_Some @@ Path.of_string s

let ok_or_fail = function
  | Ok x -> x
  | Error s -> failwith s
                 
let save_to_dot name c = to_file name (Debug.dot_of_cursor c)

let path = path_of_string
let value = Value.of_string
