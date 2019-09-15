(* Human friendly string file/directory names => segment 

   222 is the maximum length allowed in segment
*)
  
open Segment
open Result
    
type t = string

(*
   '/' is invalid character in key.

   If the key is too long (more than 222 bits) for a single segment,
   it is splitted into multiple segments seprated by Buds.
*)

type kind =
  | Hex      (* 0001.. : Hex, 4bits per char. *)
  | Lower    (* 001..  : [a-z \-.:=_]*, 5bits per char. *)
  | Alphanum (* 01..   : [0-9A-Za-z-_]*, 6bits per char. *)
  | Ascii    (* 1..    : ASCII, 7bits per char. *)
  | Binary   (* 0000.. : Binary, 8bits per char. *)

let kind_of_key key = 
  let len = String.length key in
  let rec f hex lower alphanum i =
    if i = len then 
      match hex, lower, alphanum with
      | true, _, _ -> Hex
      | _, true, _ -> Lower
      | _, _, true -> Alphanum
      | _ -> Ascii
    else
      let c = String.unsafe_get key i in
      let x = Char.code c in
      match c with
      | '0'..'9' -> f hex false alphanum (i+1)
      | 'a'..'f' -> f hex lower alphanum (i+1)
      | 'g'..'z' | '-' | '_' -> f false lower alphanum (i+1)
      | ' ' | '.' | ':' | '=' -> f false lower false (i+1)
      | 'A'..'Z' -> f false false alphanum (i+1)
      | _ when x < 128 -> f false false false (i+1)
      | _ -> Binary
  in
  f true true true 0
    
(* 
   Length info must be included to allow having keys which one is a prefix
   of the other can co-exist in the tree, i.e.  "hell" and "hello".

   length encoding 

   1x            1-2
   01xxxx        3-18
   001xxxxxxxx   19-274
*)

let bit x n = if x land n = 0 then Left else Right
let unbit x = function
  | Left -> 0
  | Right -> x

let encode_length n =
  match n with
  | 1 -> Ok [ Right ; Left ] (* 10 *)
  | 2 -> Ok [ Right ; Right ] (* 11 *)
  | _ when n < 1 -> Error "0 or negative length"
  | _ ->
      if n < 19 then
        let x = n - 3 in
        Ok [ Left ; Right ;
             bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ]
      else if n < 275 then
        let x = n - 19 in
        Ok [ Left ; Left ; Right ;
             bit x 128 ; bit x 64 ; bit x 32 ; bit x 16 ;
             bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ]
      else Error "key length too big"

let decode_length = function
  | Right :: Left :: seg -> Ok (1, seg)
  | Right :: Right :: seg -> Ok (2, seg)
  | Left :: Right :: b8 :: b4 :: b2 :: b1 :: seg ->
      Ok (unbit 8 b8 + unbit 4 b4 + unbit 2 b2 + unbit 1 b1 + 3, seg)
  | Left :: Left :: Right :: b128 :: b64 :: b32 :: b16 :: b8 :: b4 :: b2 :: b1 :: seg ->
      Ok (unbit 128 b128 + unbit 64 b64 + unbit 32 b32 + unbit 16 b16
          + unbit 8 b8 + unbit 4 b4 + unbit 2 b2 + unbit 1 b1 + 19, seg)
  | _ -> Error "invalid key length"
        
let encode_hex key =
  let len = String.length key in
  encode_length len >>= fun l ->
  let rev_head = List.rev_append l [ Right ; Left ; Left ; Left ] in
  let rec f rev_st i =
    if i = len then List.rev rev_st
    else
      let c = String.unsafe_get key i in
      let c' = Char.code c in
      let x = match c with
        | '0' .. '9' -> c' - Char.code '0'
        | 'a' .. 'f' -> c' - Char.code 'a' + 10
        | _ -> assert false
      in
      f (List.rev_append [ bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
  in
  Ok (f rev_head 0)

let encode_lower key = 
  let len = String.length key in
  encode_length len >>= fun l ->
  let rev_head = List.rev_append l [ Right ; Left ; Left ] in
  let rec f rev_st i =
    if i = len then List.rev rev_st
    else
      let c = String.unsafe_get key i in
      let c' = Char.code c in
      let x = match c with
        | 'a' .. 'z' -> c' - Char.code 'a'
        | ' ' -> 26
        | '-' -> 27
        | '.' -> 28
        | ':' -> 29
        | '=' -> 30
        | '_' -> 31
        | _ -> assert false
      in
      f (List.rev_append [ bit x 16 ; bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
  in
  Ok (f rev_head 0)

let encode_alphanum key =
  let len = String.length key in
  encode_length len >>= fun l ->
  let rev_head = List.rev_append l [ Right ; Left ] in
  let rec f rev_st i =
    if i = len then List.rev rev_st
    else
      let c = String.unsafe_get key i in
      let c' = Char.code c in
      let x = match c with
        | '0' .. '9' -> c' - Char.code '0'
        | 'A' .. 'Z' -> c' - Char.code 'A' + 10
        | 'a' .. 'z' -> c' - Char.code 'a' + 36
        | '-' -> 62
        | '_' -> 63
        | _ -> assert false
      in
      f (List.rev_append [ bit x 32 ; bit x 16 ; bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
  in
  Ok (f rev_head 0)

let encode_ascii key =
  let len = String.length key in
  encode_length len >>= fun l ->
  let rev_head = List.rev_append l [ Right ] in
  let rec f rev_st i =
    if i = len then List.rev rev_st
    else
      let c = String.unsafe_get key i in
      let x = Char.code c in
      if x >= 128 then assert false;
      f (List.rev_append [ bit x 64 ; bit x 32 ; bit x 16 ; bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
  in
  Ok (f rev_head 0)
    
let encode_binary key =
  let len = String.length key in
  encode_length len >>= fun l ->
  let rev_head = List.rev_append l [ Left ; Left ; Left ; Left ] in
  let len = String.length key in
  let rec f rev_st i =
    if i = len then List.rev rev_st
    else
      let c = String.unsafe_get key i in
      let x = Char.code c in
      f (List.rev_append [ bit x 128 ; bit x 64 ; bit x 32 ; bit x 16 ; bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
  in
  Ok (f rev_head 0)

let to_segment key =
  let encode = match kind_of_key key with
    | Hex -> encode_hex
    | Lower -> encode_lower
    | Alphanum -> encode_alphanum
    | Ascii -> encode_ascii
    | Binary -> encode_binary
  in
  encode key

let get_contents buf len =
  if Buffer.length buf = len then Ok (Buffer.contents buf) 
  else Error (Printf.sprintf "strange length (should be %d) %d %S"
                len (Buffer.length buf) (Buffer.contents buf))

let decode_ascii len seg =
  let buf = Buffer.create len in
  let rec f = function
    | [] -> get_contents buf len
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: seg ->
        let byte = 
          unbit 64 x1
          + unbit 32 x2
          + unbit 16 x3
          + unbit 8 x4
          + unbit 4 x5
          + unbit 2 x6
          + unbit 1 x7
        in
        Buffer.add_char buf (Char.chr byte);
        f seg
    | _ -> Error "strange length (ascii)"
  in
  f seg

let decode_alphanum len seg =
  let buf = Buffer.create len in
  let rec f = function
    | [] -> get_contents buf len
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: seg ->
        let byte = 
          unbit 32 x1
          + unbit 16 x2
          + unbit 8 x3
          + unbit 4 x4
          + unbit 2 x5
          + unbit 1 x6
        in
        let c = 
          if byte < 10 then Char.chr (byte + Char.code '0')
          else if byte < 36 then Char.chr (byte + Char.code 'A' - 10)
          else if byte < 62 then Char.chr (byte + Char.code 'a' - 36)
          else 
            match byte with
            | 62 -> '-'
            | 63 -> '_'
            | _ -> assert false
        in
        Buffer.add_char buf c;
        f seg
    | _ -> Error "strange length (alphanum)"
  in
  f seg

let decode_lower len seg =
  let buf = Buffer.create len in
  let rec f = function
    | [] -> get_contents buf len
    | x1 :: x2 :: x3 :: x4 :: x5 :: seg ->
        let byte = 
          unbit 16 x1
          + unbit 8 x2
          + unbit 4 x3
          + unbit 2 x4
          + unbit 1 x5
        in
        let c = 
          if byte < 26 then Char.chr (byte + Char.code 'a')
          else
            match byte with
            | 26 -> ' '
            | 27 -> '-'
            | 28 -> '.'
            | 29 -> ':'
            | 30 -> '='
            | 31 -> '_'
            | _ -> assert false
        in
        Buffer.add_char buf c;
        f seg
    | _ -> Error "strange length (lower)"
  in
  f seg

let decode_hex len seg =
  let buf = Buffer.create len in
  let rec f = function
    | [] -> get_contents buf len
    | x1 :: x2 :: x3 :: x4 :: seg ->
        let byte = 
          unbit 8 x1
          + unbit 4 x2
          + unbit 2 x3
          + unbit 1 x4
        in
        let c = 
          if byte < 10 then Char.chr (byte + Char.code '0')
          else Char.chr (byte + Char.code 'a' - 10)
        in
        Buffer.add_char buf c;
        f seg
    | _ -> Error "strange length (hex)"
  in
  f seg

let decode_binary len seg =
  let buf = Buffer.create len in
  let rec f = function
    | [] -> get_contents buf len
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: seg ->
        let byte = 
          unbit 128 x1
          + unbit 64 x2
          + unbit 32 x3
          + unbit 16 x4
          + unbit 8 x5
          + unbit 4 x6
          + unbit 2 x7
          + unbit 1 x8
        in
        Buffer.add_char buf (Char.chr byte);
        f seg
    | _ -> Error "strange length (binary)"
  in
  f seg

let of_segment seg = match seg with
  | Right :: seg ->
      (* 1..  : ASCII *)
      decode_length seg >>= fun (len, seg) ->
      decode_ascii len seg

  | Left :: Right :: seg ->
      (* 01.. : Alphanum *)
      decode_length seg >>= fun (len, seg) ->
      decode_alphanum len seg

  | Left :: Left :: Right :: seg ->
      (* 001.. : Lower *)
      decode_length seg >>= fun (len, seg) ->
      decode_lower len seg

  | Left :: Left :: Left :: Right :: seg ->
      (* 0001.. : Hex *)
      decode_length seg >>= fun (len, seg) ->
      decode_hex len seg

  | Left :: Left :: Left :: Left :: seg ->
      (* 00001.. : Binary *)
      decode_length seg >>= fun (len, seg) ->
      decode_binary len seg

  | _ -> Error ("strange header: " ^ Segment.(to_string @@ of_side_list seg))

let take n xs =
  let rec take n st xs = match n, xs with
    | 0, _ -> List.rev st, xs
    | n, x::xs -> take (n-1) (x::st) xs
    | _, [] -> assert false
  in
  take n [] xs

(*
   220 or less : The key ends here.
   222 : The key ends here, using 221 bits.  The last 222nd is 0.
   221 : The key does not end here and continues to the next level
*)
let to_segments key =
  to_segment key >>= fun seg ->
  let length = List.length seg in
  let rec f rev_segs length seg =
    if length <= 220 then List.rev (Segment.of_side_list seg :: rev_segs)
    else if length = 221 then List.rev (Segment.of_side_list (seg @ [Left]) :: rev_segs)
    else 
      let seg221, rest = take 221 seg in
      f (Segment.of_side_list seg221 :: rev_segs) (length - 221) rest
  in
  Ok (f [] length seg)

let of_segments segs =
  let rec check = function
    | [] -> Error "empty"
    | [seg] -> 
        let seg = Segment.to_side_list seg in
        let len = List.length seg in
        if len = 222 then Ok [fst @@ take 221 seg]
        else if len <= 220 then Ok [seg]
        else Error "invalid terminal"
    | seg::segs ->
        let seg = Segment.to_side_list seg in
        if List.length seg <> 221 then Error "invalid init"
        else check segs >>= fun segs -> Ok (seg :: segs)
  in
  check segs >>= fun segs ->
  of_segment @@ List.concat segs
    
