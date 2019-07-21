open Plebeia.Impl

include Debug
include Utils
    
let timed f = 
  let t1 = Unix.gettimeofday () in
  let res = Exn.catch f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

module RS = Random.State
              
let random_string st len = String.init len (fun _ -> Char.chr @@ RS.int st 256)
    
let random_segment ?length st =
  let open Segment in
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

let to_file = Utils.to_file

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

let tempfile = Filename.temp_file "plebeia" ".context"

let () = Format.eprintf "Using temp file %s ...@." tempfile

let test_with_context length f =
  let context = Context.create ~length tempfile in
  let res = f context in
  Context.close context;
  res
  
let test_with_cursor f =
  test_with_context 1000000 (fun context ->
    let cursor = Cursor.empty context in
    f cursor)
    
let path_of_string s = from_Some @@ Segment.of_string s

let ok_or_fail = function
  | Ok x -> x
  | Error s -> failwith s

let must_fail = function
  | Ok _ -> failwith "must fail"
  | Error _ -> ()
                 
let path = path_of_string
let value = Value.of_string
