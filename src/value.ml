type t = string
let of_string s = s
let to_string s = s
let length = String.length
let to_hex s = Hex.of_string s
let to_hex_string s = Hex.show @@ Hex.of_string s

