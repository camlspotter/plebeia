type t = (Hash.hash28, Value.t * int) Hashtbl.t

let make () = Hashtbl.create 0

let close _ = ()

let insert table h v =
  match Hashtbl.find_opt table h with
  | None -> Hashtbl.add table h (v, 1)
  | Some (v', _) when v <> v' -> assert false (* XXX unfortunate hash collision! *)
  | Some (_, count) -> Hashtbl.replace table h (v, count+1)

let get_opt table h =
  match Hashtbl.find_opt table h with
  | None -> None
  | Some (v, _) -> Some v

let decr table h =
  match Hashtbl.find_opt table h with
  | None -> Printf.printf "none\n" ; ()
  | Some (_, 1) -> Printf.printf "some 1\n" ; Hashtbl.remove table h
  | Some (v, count) -> Printf.printf "some %d\n" count ;
      Hashtbl.replace table h (v, count-1)
