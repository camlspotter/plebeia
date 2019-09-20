type side = Left | Right

let string_of_side = function
  | Left -> "L"
  | Right -> "R"

type segment = 
  | List of side list
  | Cons of side * segment
  | Append of segment * segment
  | Encoding of int (* len *) * string (* non null encoded *)

type t = segment

let empty = List []

let of_side_list l = List l

let rec is_empty = function
  | List [] -> true
  | List _ -> false
  | Cons _ -> false
  | Append (seg1, seg2) -> is_empty seg1 && is_empty seg2 (* we should exclude this *)
  | Encoding (0, _) -> true (* we should exclude this *)
  | Encoding _ -> false
    
let rec length = function
  | List ss -> List.length ss
  | Cons (_, seg) -> length seg + 1
  | Append (seg1, seg2) -> length seg1 + length seg2
  | Encoding (len, _) -> len

let cons x xs = Cons (x, xs)

let append seg1 seg2 = match seg1, seg2 with
  | List xs1, List xs2 -> List (xs1 @ xs2)
  | _ -> 
      if is_empty seg1 then seg2
      else if is_empty seg2 then seg1
      else Append (seg1, seg2)
      
let rec concat = function
  | [] -> List []
  | seg::segs -> append seg @@ concat segs
      
let decode_ h =
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
  clean @@ f 0

let length_of_encoded s =
  let rec find_the_first_one pos =
    if pos >= 28 then assert false
    else 
    let c = Char.code @@ String.unsafe_get s pos in
    if c = 0 then find_the_first_one (pos + 1)
    else 
      pos * 8 
      + if c land 0x80 <> 0 then 0
        else if c land 0x40 <> 0 then 1
        else if c land 0x20 <> 0 then 2
        else if c land 0x10 <> 0 then 3
        else if c land 0x8 <> 0 then 4
        else if c land 0x4 <> 0 then 5
        else if c land 0x2 <> 0 then 6
        else 7
  in
  let pos = find_the_first_one 0 in
  224 - pos + 2 

let rec cut = function
  | List [] -> None
  | List (x::xs) -> Some (x, List xs)
  | Cons (x,seg) -> Some (x, seg)
  | Append (seg1, seg2) when is_empty seg1 -> cut seg2
  | Append (seg1, seg2) -> 
      begin match cut seg1 with
      | None -> assert false
      | Some (side, seg1) when is_empty seg1 -> Some (side, seg2)
      | Some (side, seg1) -> Some (side, Append (seg1, seg2))
      end
  | Encoding (0, _) -> None
  | Encoding (_, s) -> 
      match decode_ s with
      | [] -> assert false
      | s::seg -> Some (s, List seg)

let rec to_side_list seg =
  match seg with
  | List ss -> ss
  | Cons (s, seg) -> s :: to_side_list seg
  | Append (seg1, seg2) -> to_side_list seg1 @ to_side_list seg2
  | Encoding (_, s) -> decode_ s

let rec equal seg1 seg2 =
  seg1 == seg2 || 
  match seg1, seg2 with
  | List ss1, List ss2 -> ss1 = ss2
  | Cons (s1, seg1), Cons (s2, seg2) ->
      s1 = s2 && equal seg1 seg2
  | Encoding (_, s), Encoding (_, s') -> s = s'
  | _ -> to_side_list seg1 = to_side_list seg2
  
let encode_ seg =
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
    | seg -> Format.eprintf "make_byte: %d@." (List.length seg);  assert false
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
  assert (bit_pos < 8);
  fill_bytes byte_pos (
    (List.init bit_pos (fun _ -> Left))
    @ Right :: to_side_list seg
    @ [ Right ]
  ) (* XXX inefficient! *)

let encode = function
  | Encoding (_, s) -> s
  | seg -> encode_ @@ List (to_side_list seg)
    
let to_string s = String.concat "" (List.map string_of_side @@ to_side_list s)

let of_string s =
  let rec aux st = function
    | -1 -> Some (of_side_list st)
    | n -> 
        match String.unsafe_get s n with
        | 'L' -> aux (Left :: st) (n-1)
        | 'R' -> aux (Right :: st) (n-1)
        | _ -> None
  in
  aux [] @@ String.length s - 1

let rec common_prefix seg1 seg2 = match (cut seg1, cut seg2) with
  | (Some (h1, t1), Some (h2, t2)) ->
    if h1 = h2 then
      let (prefix, r1, r2) = common_prefix t1 t2 in
      (h1 :: prefix, r1, r2)
    else
      ([], seg1, seg2)
  | (None, _) -> ([], empty, seg2)
  | (_, None) -> ([], seg1, empty)

let common_prefix seg1 seg2 = 
  let sides, seg2, seg3 = common_prefix seg1 seg2 in
  of_side_list sides, seg2, seg3

let decode s = 
  let len = length_of_encoded s in
  Encoding (len, s)
