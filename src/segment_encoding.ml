open Path

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

let encode (seg : segment) =
  let seg = (seg :> side list) in
  let len = List.length seg in
  if len > 222 then failwith "segment is too long";
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
  fill_bytes byte_pos (List.init bit_pos (fun _ -> Left) @ Right :: seg @ [ Right ]) (* XXX <= inefficient! *)
