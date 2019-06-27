open Hash
open Node

let of_leaf v =
  extend_to_t @@ hash_list [ "\000"; Value.to_string v]

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
  extend_to_t 
  @@ reset_last_2bits 
  @@ hash_list [ "\001"; to_string l; to_string r ]

(*
   |<-                       H_child                           ->|
   | The first 224bits of H_child |0......01|<- segment bits ->|1|
*) 
let of_extender seg h =
  hash56_of_string (String.sub (to_string h) 0 28 ^ Segment_encoding.encode seg)

let of_extender' ~segment_code (h : t) =
  hash56_of_string (String.sub (to_string h) 0 28 ^ segment_code)



let hash (Cursor (trail, node, context)) =
  let rec hash_aux : node -> (node * t) = function
    | Disk (index, wit) -> 
        let v, h = hash_aux' (load_node context index wit) in View v, h
    | View v -> 
        let v, h = hash_aux' v in View v, h

  and hash_aux' : view -> (view * t) = fun v -> 
    match v with
    (* easy case where it's already hashed *)
    | Leaf (_, _, Hashed h) -> (v, h)
    | Bud (_, _, Hashed h) -> (v, h)
    | Internal (_, _, _, Hashed h)  -> (v, h)
    | Extender (_, _, _, Hashed h) -> (v, h)

    (* hashing is necessary below *)
    | Leaf (v, _, Not_Hashed) -> 
        let h = of_leaf v in
        (_Leaf (v, Not_Indexed, Hashed h), h)

    | Bud (Some underneath, _, Not_Hashed) ->
        let (node, h) = hash_aux underneath in
        (_Bud (Some node, Not_Indexed, Hashed h), h)

    | Bud (None, _, Not_Hashed) ->
        let h = of_empty_bud in
        (_Bud (None, Not_Indexed, Hashed h), h)

    | Internal (left, right, Left_Not_Indexed, Not_Hashed) -> (
        let (left, hl) = hash_aux left
        and (right, hr) = hash_aux right in
        (*
           |<-     H(0x01 || l || h)    ->|
           |                           |00|0...........................01|
        *)
        let h = of_internal hl hr in
        (_Internal (left, right, Left_Not_Indexed, Hashed h), h))

    | Internal (left, right, Right_Not_Indexed, Not_Hashed) -> (
        let (left, hl) = hash_aux left
        and (right, hr) = hash_aux right in
        (*
           |<-     H(0x01 || l || h)    ->|
           |                           |00|0...........................01|
        *)
        let h = of_internal hl hr in
        (_Internal (left, right, Right_Not_Indexed, Hashed h), h))

    | Extender (segment, underneath, _, Not_Hashed)  ->
        let (underneath, h) = hash_aux underneath in
        (*
           |<-                       H_child                           ->|
           | The first 224bits of H_child |0......01|<- segment bits ->|1|
        *) 
        let h = of_extender segment h in
        (_Extender (segment, underneath, Not_Indexed, Hashed h), h)

    | Internal (_, _, (Not_Indexed|Indexed _), Not_Hashed) -> assert false
  in 
  let (node, h) =  hash_aux node in
  (_Cursor (trail, node, context), h)
