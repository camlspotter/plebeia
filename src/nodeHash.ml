open Hash
open Node

let of_leaf v =
  extend_to_hash56 @@ hash_list [ "\000"; Value.to_string v]

(* XXX correct? *)
let of_empty_bud = hash56_of_string @@ String.make 56 '\000'

(* the hash of a bud node is the hash of its child *)
let of_bud = function
  | None -> of_empty_bud
  | Some h -> h

(*
   |<-     H(0x01 || l || h)    ->|
   |                          |0|0|0...........................01|
*)
let of_internal l r =
  extend_to_hash56 
  @@ reset_last_2bits 
  @@ hash_list [ "\001"; to_string l; to_string r ]

(*
   |<-                       H_child                           ->|
   | The first 224bits of H_child |0......01|<- segment bits ->|1|
*) 
let of_extender seg h =
  hash56_of_string (String.sub (to_string h) 0 28 ^ Segment_encoding.encode seg)

let of_extender' ~segment_code (h : hash56) =
  hash56_of_string (String.sub (to_string h) 0 28 ^ segment_code)



let hash (Cursor (trail, node, context)) =
  let rec hash_aux : node -> (node * hash56) = function
    | Disk (index, wit) -> 
        let v, h = hash_aux' (load_node context index wit) in View v, h
    | View v -> 
        let v, h = hash_aux' v in View v, h

  and hash_aux' : view -> (view * hash56) = fun v -> 
    match v with
    (* easy case where it's already hashed *)
    | Leaf (_, _, Hashed h, _) -> (v, h)
    | Bud (_, _, Hashed h, _) -> (v, h)
    | Internal (_, _, _, Hashed h, _)  -> (v, h)
    | Extender (_, _, _, Hashed h, _) -> (v, h)

    (* hashing is necessary below *)
    | Leaf (v, _, Not_Hashed, _) -> 
        let h = of_leaf v in
        (_Leaf (v, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Bud (Some underneath, _, Not_Hashed, _) ->
        let (node, h) = hash_aux underneath in
        (_Bud (Some node, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Bud (None, _, Not_Hashed, _) ->
        let h = of_empty_bud in
        (_Bud (None, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Internal (left, right, Left_Not_Indexed, Not_Hashed, _) -> (
        let (left, hl) = hash_aux left
        and (right, hr) = hash_aux right in
        (*
           |<-     H(0x01 || l || h)    ->|
           |                           |00|0...........................01|
        *)
        let h = of_internal hl hr in
        (_Internal (left, right, Left_Not_Indexed, Hashed h, Not_Indexed_Any), h))

    | Internal (left, right, Right_Not_Indexed, Not_Hashed, _) -> (
        let (left, hl) = hash_aux left
        and (right, hr) = hash_aux right in
        (*
           |<-     H(0x01 || l || h)    ->|
           |                           |00|0...........................01|
        *)
        let h = of_internal hl hr in
        (_Internal (left, right, Right_Not_Indexed, Hashed h, Not_Indexed_Any), h))

    | Extender (segment, underneath, _, Not_Hashed, _)  ->
        let (underneath, h) = hash_aux underneath in
        (*
           |<-                       H_child                           ->|
           | The first 224bits of H_child |0......01|<- segment bits ->|1|
        *) 
        let h = of_extender segment h in
        (_Extender (segment, underneath, Not_Indexed, Hashed h, Not_Indexed_Any), h)

    | Internal (_, _, (Not_Indexed|Indexed _), Not_Hashed, _) -> assert false
  in 
  let (node, h) =  hash_aux node in
  (_Cursor (trail, node, context), h)
