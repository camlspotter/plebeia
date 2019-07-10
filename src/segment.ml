type side = Left | Right

let dummy_side = Left

type segment = side list (* FIXME: use bit arithmetic *)

type t = segment

let is_empty x = x = []

let cut = function
  | [] -> None
  | h::t -> Some (h,t)

let empty = []

let concat = (@)

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
