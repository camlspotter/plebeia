type side = Left | Right

type segment = side list (* FIXME: use bit arithmetic *)

type t = segment

let is_empty x = x = []

let cut = function
  | [] -> None
  | h::t -> Some (h,t)

let empty = []

let append = (@)
let concat = List.concat
let length = List.length
  
let string_of_side = function
  | Left -> "L"
  | Right -> "R"

let to_string s =
  String.concat "" (List.map string_of_side s)

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
let to_side_list x = x
let cons x xs = x :: xs

let decode h =
  assert (String.length h = 28);
  let sides_of_char c = 
    let c = Char.code c in
    let f x = if c land x = 0 then Left else Right in
    [f 128; f 64; f 32; f 16; f 8; f 4; f 2; f 1]
  in
  let sides_of_char_last c = 
    let c = Char.code c in
    let f x = if c land x = 0 then Left else Right in
    [f 128; f 64; f 32; f 16; f 8; f 4; f 2]
  in
  let rec f = function
    | 28 -> []
    | 27 as pos -> sides_of_char_last (String.unsafe_get h pos) @ f (pos+1)
    | pos -> sides_of_char (String.unsafe_get h pos) @ f (pos+1)
  in
  let rec clean = function
    | [] -> assert false
    | Right :: seg -> seg
    | Left :: seg -> clean seg
  in
  of_side_list @@ clean @@ f 0

let encode seg =
  let len = length seg in
  if len > 222 then failwith (Printf.sprintf "segment is too long (%d)" len);
  let head_zero_bits = 224 - len - 2 in
  let head_zero_bytes = head_zero_bits / 8 in
  let bytes = Bytes.make 28 '\000' in
  let byte_pos = head_zero_bytes in
  let bit_pos = head_zero_bits mod 8 in
  let make_byte = function
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
        (Char.chr byte, seg)
    | _ -> assert false
  in
  let rec fill_bytes byte_pos = function
    | [] -> 
        assert (byte_pos = 28);
        Bytes.to_string bytes
    | seg ->
        let (c, seg) = make_byte seg in
        Bytes.unsafe_set bytes byte_pos c;
        let byte_pos' = byte_pos + 1 in
        if byte_pos' > 28 then assert false; (* segment is too long *)
        fill_bytes byte_pos' seg
  in
  fill_bytes byte_pos (
    (List.init bit_pos (fun _ -> Left))
    @ Right :: to_side_list seg
    @ [ Right ]
  ) (* XXX inefficient! *)
