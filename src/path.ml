type side = Left | Right
let dummy_side = Left
type segment = side list (* FIXME: use bit arithmetic *)
let is_empty x = x = []
let cut = function
  | [] -> None
  | h::t -> Some (h,t)
let empty = []

let concat = (@)

let to_string s =
  String.concat "" (List.map (function Left -> "L" | Right -> "R") s)

let of_string s =
  let rec aux st = function
    | -1 -> Some st
    | n -> 
        match String.unsafe_get s n with
        | 'L' -> aux (Left :: st) (n-1)
        | 'R' -> aux (Right :: st) (n-1)
        | _ -> None
  in
  aux [] @@ String.length s - 1

let rec common_prefix seg1 seg2 = match (seg1, seg2) with
  | (h1 :: t1, h2 :: t2) ->
    if h1 = h2 then
      let (prefix, r1, r2) = common_prefix t1 t2 in
      (h1 :: prefix, r1, r2)
    else
      ([], seg1, seg2)
  | ([], _) -> ([], [], seg2)
  | (_, []) -> ([], seg1, [])

let of_side_list l = l

(* XXXX quite similar to Segment_encoding.encode *)
let to_key seg =
  let buf = Buffer.create 128 in
  let rec f = function
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: seg ->
        let bit = function
          | Left -> 0
          | Right -> 1
        in
        let byte = 
          (bit x1) lsl 7
          + (bit x2) lsl 6
          + (bit x3) lsl 5
          + (bit x4) lsl 4
          + (bit x5) lsl 3
          + (bit x6) lsl 2
          + (bit x7) lsl 1
          + (bit x8) * 1
        in
        Buffer.add_char buf (Char.chr byte);
        f seg
    | [] -> Some (Buffer.contents buf)
    | _ -> None (* XXX error detail *)
  in
  f seg

let to_key seg =
  let buf = Buffer.create 128 in
  let rec f = function
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: seg ->
        let bit = function
          | Left -> 0
          | Right -> 1
        in
        let byte = 
          (bit x1) lsl 7
          + (bit x2) lsl 6
          + (bit x3) lsl 5
          + (bit x4) lsl 4
          + (bit x5) lsl 3
          + (bit x6) lsl 2
          + (bit x7) lsl 1
          + (bit x8) * 1
        in
        Buffer.add_char buf (Char.chr byte);
        f seg
    | [] -> Some (Buffer.contents buf)
    | _ -> None (* XXX error detail *)
  in
  f seg
    
let of_key s = 
  assert (s <> "");
  let rec f acc i =
    if i < 0 then acc
    else
      let c = Char.code @@ String.unsafe_get s i in
      let p n = if c land n = 0 then Left else Right in
      f ([ p 128 ; p 64; p 32; p 16; p 8; p 4; p 2; p 1 ] @ acc) (i - 1)
  in
  f [] (String.length s - 1)

let () =
  assert (to_key @@ of_key "hello world" = Some "hello world")
