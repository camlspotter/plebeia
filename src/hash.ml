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
  assert (Hex.of_string @@ Blake2B_28.of_string "Replacing SHA1 with the more secure function"
          = `Hex "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578");
  assert (Hex.of_string @@ Blake2B_28.of_strings ["Replacing SHA1 with the "; "more secure function" ]
          = `Hex "6bceca710717901183f66a78a7a9f59441c56defcff32f33f1e1b578")
(* obtained by
#!/usr/local/bin/python3

from hashlib import blake2b

h = blake2b(digest_size=28)
h.update(b'Replacing SHA1 with the more secure function')
print (h.hexdigest())
*)

let hash = Blake2B_28.of_string
let hash_list = Blake2B_28.of_strings

type t = string

let to_string x = x

let of_string x = assert (String.length x = 28); x
(* This module is only for internal use.
   Error recovery is not required. *) 

let to_hex x = Hex.of_string x
let to_hex_string x = Hex.show @@ Hex.of_string x
let of_hex = Hex.to_string
  
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

let zero = String.make 28 '\000'
