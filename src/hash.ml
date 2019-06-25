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

let hash = Blake2B_28.of_string
let hash_list = Blake2B_28.of_strings

type h28
type h56

type 'a gen = string

type hash28 = h28 gen
type hash56 = h56 gen
type t = hash56

let to_string x = x
let hash28_of_string x = assert (String.length x = 28); x
let hash56_of_string x = assert (String.length x = 56); x

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

let extend_to_t h =
  hash56_of_string
    (h ^ "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001")

let shorten_to_hash28 h = String.sub h 0 28 
