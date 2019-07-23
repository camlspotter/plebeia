open Hash
open Node

type t = LH of Hash.t * string

let to_string (LH (h,x)) = Hash.to_string h ^ x

let shorten (LH (h,_)) = h
    
let make h postfix =
  assert (String.length postfix = 28);
  LH (h, postfix)

let zero = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
let h_zero = Hash.of_string zero

let lh_zero = make h_zero zero
let one = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001"

let of_bud = function
  | None -> lh_zero
  | Some lh -> lh

(*
   |<-     H(0x01 || l || h)    ->|
   |                          |0|0|0...........................01|
*)
let of_internal_hash h = make h one
let of_internal l r =
  of_internal_hash
  @@ reset_last_2bits 
  @@ hash_list [ "\001"; to_string l; to_string r ]

let of_leaf_hash h = make h one

let of_leaf v =
  let h = hash_list [ "\000"; Value.to_string v] in
  of_leaf_hash h

(*
   |<-                       H_child                           ->|
   | The first 224bits of H_child |0......01|<- segment bits ->|1|
*) 
let of_extender_hash seg h = make h (Segment_encoding.encode seg)
let of_extender seg lh =
  make (shorten lh) (Segment_encoding.encode seg)
let of_extender' ~segment_code lh =
  make (shorten lh) segment_code

let long_hash context node : (view * t) =
  let rec aux : node -> (view * t) = function
    | Disk (index, wit) -> 
        let v, lh = aux' (load_node context index wit) in v, lh
    | View v -> 
        let v, lh = aux' v in v, lh

  and aux' : view -> (view * t) = fun v -> 
    match v with
    (* easy case where it's already hashed *)
    | Leaf (_, _, Hashed h) -> (v, of_leaf_hash h)
    | Bud (None, _, Hashed _h) -> (v, of_bud None)
    | Bud (Some n, _, Hashed _h) -> (v, of_bud @@ Some (snd @@ aux n))
    | Internal (_, _, _, Hashed h)  -> (v, of_internal_hash h)
    | Extender (seg, _, _, Hashed h) -> (v, of_extender_hash seg h)

    (* from here, hashing is necessary *)

    | Leaf (v, _, Not_Hashed) -> 
        let lh = of_leaf v in
        let h = shorten lh in
        (_Leaf (v, Not_Indexed, Hashed h), lh)

    | Bud (Some underneath, _, Not_Hashed) ->
        let (v, lh) = aux underneath in
        let lh = of_bud @@ Some lh in
        let h = shorten lh in
        (_Bud (Some (View v), Not_Indexed, Hashed h), lh)

    | Bud (None, _, Not_Hashed) ->
        let lh = of_bud None in
        let h = shorten lh in
        (_Bud (None, Not_Indexed, Hashed h), lh)

    | Internal (left, right, Not_Indexed, Not_Hashed) -> 
        let (left, lhl) = aux left
        and (right, lhr) = aux right in
        let lh = of_internal lhl lhr in
        let h = shorten lh in
        (_Internal (View left, View right, Not_Indexed, Hashed h), lh)

    | Extender (segment, underneath, _, Not_Hashed)  ->
        let (underneath, lh) = aux underneath in
        let lh = of_extender segment lh in
        let h = shorten lh in
        (_Extender (segment, View underneath, Not_Indexed, Hashed h), lh)

    | Internal (_, _, Indexed _, Not_Hashed) -> assert false
  in 
  aux node

let hash c n = 
  let v, lh = long_hash c n in
  v, shorten lh

(*
let hash (Cursor (trail, node, context)) = 
  let v, h = Node_hash.hash context node in
  _Cursor (trail, View v, context), h
*)
