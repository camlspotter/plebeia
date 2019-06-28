(* In memory KVS *)

type t = (Hash.hash28, Value.t * int) Hashtbl.t

let make () = Hashtbl.create 0

let close _ = ()

let insert table h v =
  match Hashtbl.find_opt table h with
  | None -> Hashtbl.add table h (v, 1)
  | Some (v', _) when v <> v' -> 
      (* XXX unfortunate hash collision! 
         If this happens, we cannot store [v] in the KVS at all.
      *)
      assert false 
  | Some (_, count) -> Hashtbl.replace table h (v, count+1)

let get_opt table h =
  match Hashtbl.find_opt table h with
  | None -> None
  | Some (v, _) -> Some v

let decr table h =
  match Hashtbl.find_opt table h with
  | None -> ()
  | Some (_, 1) -> Hashtbl.remove table h
  | Some (v, count) -> Hashtbl.replace table h (v, count-1)
