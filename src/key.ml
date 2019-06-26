(* Human friendly string file/directory names => segment 

   222 is the maximum length allowed in segment
*)
  
open Segment
    
type t = string

(*
   The first byte is for the kind and length.
   Length info must be included to allow having keys which one is a prefix
   of the other can co-exist in the tree, i.e.  "hell" and "hello".

   '/' is invalid character in key.
   
   1xxxxx..      : ASCII, 7bits per char. Max: (222 - 6) / 7 = 30
   01xxxxxx.     : [0-9A-Za-z-_]*, 6bits per char. Max: (222 - 7) / 6 = 35
   001xxxxxx..   : [a-z \-.:=_]*, 5bits per char.  Max: (222 - 09) / 5 = 42
   0001xxxxxx..  : Hex, 4bits per char.  Max: (222 - 10) / 4 = 53
   00001xxxxx.. : Binary, 8bits per char. Max: (222 - 10) / 8 = 26
   
*)

type kind =
  | Hex | Lower | Alphanum | Ascii | Binary
  (* Do not reorder the constructors since we use the ordering *)

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
    
let to_segment key =
  let len = String.length key in 
  let bit x n = if x land n = 0 then Left else Right in 
  match kind_of_key key with
  | Hex ->
      if len > 53 then None
      else
        let rev_head =
          (* rev of 0001xxxxxx *)

          [ bit len 1 ; bit len 2 ; bit len 4 ; bit len 8 ; bit len 16 ; bit len 32 ;
            Right ; Left ; Left ; Left ]
        in
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
        Some (f rev_head 0)

  | Lower ->
      if len > 42 then None
      else
        let rev_head = 
          (* rev of 001xxxxxx *)
          [ bit len 1 ; bit len 2 ; bit len 4 ; bit len 8 ; bit len 16 ; bit len 32 ;
            Right ; Left ; Left ]
        in
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
        Some (f rev_head 0)

  | Alphanum ->
      if len > 35 then None
      else
        let rev_head = 
          (* rev of 01xxxxxx.. *)
          [ bit len 1 ; bit len 2 ; bit len 4 ; bit len 8 ; bit len 16 ; bit len 32 ;
            Right ; Left ; ]
        in
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
        Some (f rev_head 0)

  | Ascii ->
      if len > 30 then None
      else
        let rev_head = 
          (* rev 1xxxxx.. *)
          [ bit len 1 ; bit len 2 ; bit len 4 ; bit len 8 ; bit len 16 ;
            Right  ]
        in
        let rec f rev_st i =
          if i = len then List.rev rev_st
          else
            let c = String.unsafe_get key i in
            let x = Char.code c in
            if x >= 128 then assert false;
            f (List.rev_append [ bit x 64 ; bit x 32 ; bit x 16 ; bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
        in
        Some (f rev_head 0)

  | Binary ->
      if len > 26 then None
      else
        let rev_head = 
          (* rev of 00001xxxxx.. *)
          [ bit len 1 ; bit len 2 ; bit len 4 ; bit len 8 ; bit len 16 ;
            Right ; Left ; Left ; Left ; Left ]
        in
        let rec f rev_st i =
          if i = len then List.rev rev_st
          else
            let c = String.unsafe_get key i in
            let x = Char.code c in
            f (List.rev_append [ bit x 128 ; bit x 64 ; bit x 32 ; bit x 16 ; bit x 8 ; bit x 4 ; bit x 2 ; bit x 1 ] rev_st) (i+1)
        in
        Some (f rev_head 0)

          
let get_len bits seg =
  let rec f acc bits seg =
    if bits = 0 then (acc, seg)
    else match seg with
      | Left :: seg -> f (acc * 2) (bits - 1) seg
      | Right :: seg -> f (acc * 2 + 1) (bits - 1) seg
      | [] -> prerr_endline "len?"; raise Exit
  in
  f 0 bits seg
  
let of_segment seg =
  let buf = Buffer.create 64 in
  let bit = function
    | Left -> 0
    | Right -> 1
  in
  let get_contents len =
    if Buffer.length buf = len then Some (Buffer.contents buf) else begin
      Format.eprintf "strange length %d %S@." len (Buffer.contents buf);
      None
    end
  in
  try
    match seg with
    | Right :: seg ->
        (* 1xxxxx..      : ASCII, 7bits per char. Max: (222 - 6) / 7 = 30 *)
        let len, seg = get_len 5 seg in
        let rec f = function
          | [] -> get_contents len
          | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: seg ->
              let byte = 
                (bit x1) lsl 6
                + (bit x2) lsl 5
                + (bit x3) lsl 4
                + (bit x4) lsl 3
                + (bit x5) lsl 2
                + (bit x6) lsl 1
                + (bit x7)
              in
              Buffer.add_char buf (Char.chr byte);
              f seg
          | _ -> None
        in
        f seg

    | Left :: Right :: seg ->
        (* 01xxxxxx..     : [0-9A-Za-z-_]*, 6bits per char. Max: (222 - 7) / 6 = 35 *)
        let len, seg = get_len 6 seg in
        let rec f = function
          | [] -> get_contents len
          | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: seg ->
              let byte = 
                (bit x1) lsl 5
                + (bit x2) lsl 4
                + (bit x3) lsl 3
                + (bit x4) lsl 2
                + (bit x5) lsl 1
                + (bit x6)
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
          | _ -> prerr_endline "garbage alphanum"; None
        in
        f seg

    | Left :: Left :: Right :: seg ->
        (* 001xxxxxx..   : [a-z \-.:=_]*, 5bits per char.  Max: (222 - 09) / 5 = 42 *)
        let len, seg = get_len 6 seg in
        let rec f = function
          | [] -> get_contents len
          | x1 :: x2 :: x3 :: x4 :: x5 :: seg ->
              let byte = 
                (bit x1) lsl 4
                + (bit x2) lsl 3
                + (bit x3) lsl 2
                + (bit x4) lsl 1
                + (bit x5)
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
          | _ -> None
        in
        f seg

    | Left :: Left :: Left :: Right :: seg ->
        (* 0001xxxxxx..  : Hex, 4bits per char.  Max: (222 - 10) / 4 = 53 *)
        let len, seg = get_len 6 seg in
        let rec f = function
          | [] -> get_contents len
          | x1 :: x2 :: x3 :: x4 :: seg ->
              let byte = 
                (bit x1) lsl 3
                + (bit x2) lsl 2
                + (bit x3) lsl 1
                + (bit x4)
              in
              let c = 
                if byte < 10 then Char.chr (byte + Char.code '0')
                else Char.chr (byte + Char.code 'a' - 10)
              in
              Buffer.add_char buf c;
              f seg
          | _ -> None
        in
        f seg

    | Left :: Left :: Left :: Left :: Right :: seg ->
        (* 00001xxxxx.. : Binary, 8bits per char. Max: (222 - 10) / 8 = 26 *)
        let len, seg = get_len 5 seg in
        let rec f = function
          | [] -> get_contents len
          | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: seg ->
              let byte = 
                (bit x1) lsl 7
                + (bit x2) lsl 6
                + (bit x3) lsl 5
                + (bit x4) lsl 4
                + (bit x5) lsl 3
                + (bit x6) lsl 2
                + (bit x7) lsl 1
                + (bit x8)
              in
              Buffer.add_char buf (Char.chr byte);
              f seg
          | _ -> None
        in
        f seg

    | _ -> prerr_endline "strange header"; None
  with
  | Exit -> None
