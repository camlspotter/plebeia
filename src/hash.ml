open Utils

module Blake2B_28 = struct
  let of_string s =
    let open Blake2.Blake2b in
    let b = init 28 in
    update b (Bigstring.of_string s);
    let Hash bs = final b in
    Bigstring.to_string bs

  let of_strings ss =
    let open Blake2.Blake2b in
    let b = init 28 in
    List.iter (fun s -> 
        update b (Bigstring.of_string s)) ss;
    let Hash bs = final b in
    Bigstring.to_string bs
end

let to_hex s =
  (* XXX not really fast I am afraid *)
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
      Buffer.add_string buf @@ Printf.sprintf "%02x" @@ Char.code c) s;
  Buffer.contents buf

let _test =
  assert (to_hex @@ Blake2B_28.of_string "Replacing SHA1 with the more secure function"
          = "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578");
  assert (to_hex @@ Blake2B_28.of_strings ["Replacing SHA1 with the "; "more secure function" ]
          = "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578")
(* obtained by
#!/usr/local/bin/python3

from hashlib import blake2b

h = blake2b(digest_size=28)
h.update(b'Replacing SHA1 with the more secure function')
print (h.hexdigest())
*)

let hash_list xs = Blake2B_28.of_strings xs

type t = string

let to_string x = x
let of_string x = assert (String.length x = 56); x

let _reset_last_bit s =
  let len = String.length s in
  if len <> 28 then failwithf "reset_last_bit: len=%d <> 28" len; 
  let bs = Bytes.of_string s in
  Bytes.unsafe_set bs 27 
  @@ Char.chr @@ Char.code (Bytes.unsafe_get bs 27) land 0xfe;
  Bytes.to_string bs

let reset_last_2bits s =
  let len = String.length s in
  if len <> 28 then failwithf "reset_last_2bits: len=%d <> 28" len; 
  let bs = Bytes.of_string s in
  Bytes.unsafe_set bs 27 
  @@ Char.chr @@ Char.code (Bytes.unsafe_get bs 27) land 0xfc;
  Bytes.to_string bs

(* the hash of a leaf node with value v is taken as `H(0x00 || v)`, followed by a 223 0's and a 1.

   |<-      H(0x00 || v)        ->|
   |                              |0...........................01|

*)

type value_hash = string
let value_hash_of_string s = assert (String.length s = 28); s
let string_of_value_hash s = s

let of_value_hash h =
  of_string
  (h ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001")

let to_value_hash h = String.sub h 0 28 

let of_leaf v =
  of_value_hash @@ hash_list [ "\000"; Value.to_string v]


(* XXX correct? *)
let of_empty_bud = of_string (String.make 56 '\000')

(* the hash of a bud node is the hash of its child *)
let of_bud = function
  | None -> of_empty_bud
  | Some h -> h

(*
   |<-     H(0x01 || l || h)    ->|
   |                           |00|0...........................01|
*)
let of_internal_node l r =
  of_value_hash @@ reset_last_2bits @@ hash_list [ "\001"; l; r ]

let decode_segment h =
  assert (String.length h = 28);
  let sides_of_char c = 
    let c = Char.code c in
    let f x = if c land x = 0 then Path.Left else Path.Right in
    [f 128; f 64; f 32; f 16; f 8; f 4; f 2; f 1]
  in
  let sides_of_char_last c = 
    let c = Char.code c in
    let f x = if c land x = 0 then Path.Left else Path.Right in
    [f 128; f 64; f 32; f 16; f 8; f 4; f 2]
  in
  let rec f = function
    | 28 -> []
    | 27 as pos -> sides_of_char_last (String.unsafe_get h pos) @ f (pos+1)
    | pos -> sides_of_char (String.unsafe_get h pos) @ f (pos+1)
  in
  let rec clean = function
    | [] -> assert false
    | Path.Right :: seg -> seg
    | Path.Left :: seg -> clean seg
  in
  Path.of_side_list @@ clean @@ f 0

let encode_segment seg =
  let seg = (seg : Path.segment :> Path.side list) in
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
          | Path.Left -> 0
          | Path.Right -> 1
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
  fill_bytes byte_pos (List.init bit_pos (fun _ -> Path.Left) @ Path.Right :: seg @ [ Path.Right ]) (* XXX <= inefficient! *)

(*
   |<-                       H_child                           ->|
   | The first 224bits of H_child |0......01|<- segment bits ->|1|
*) 
let of_extender seg h =
  of_string (String.sub h 0 28 ^ encode_segment seg)

let of_extender' ~segment_code h =
  of_string (String.sub h 0 28 ^ segment_code)
