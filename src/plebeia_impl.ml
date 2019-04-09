(** Implementation of space-efficient binary Patricia trees in OCaml.
    The implementation is geared for used in Tezos, though it is rather
    generic. A stop-and-copy GC is provided. This implementation aims
    to maximize correctness and cares second about efficiency. Extracting
    and efficient C program from F* should be explored. *)

include Types
include Node

module Context = Context
module NodeHash = NodeHash
module Storage = Storage
module Cursor = Cursor
  
let to_disk context n =
  let n, i, h = Storage.commit_node context n in
  match n with
  | Disk _ -> n, h
  | View (Extender _) -> Disk (i, Is_Extender), h
  | View _ -> Disk (i, Not_Extender), h
  
let commit (Cursor (trail, node, context)) =
  let (node, i, h) =  Storage.commit_node context node in
  (_Cursor (trail, node, context), i, h)

let commit (Cursor (trail, _, _) as c) =
  match trail with
  | Top -> 
      let (Cursor (_ , _, context) as c), i, h = commit c in
      begin match Hashtbl.find_opt context.Context.roots_table h with
        | Some i' -> Error (c, i, h, i') (* Hash collision *)
        | None -> Hashtbl.add context.Context.roots_table h i; Ok (c, h)
      end
        
  | _ -> failwith "commit: cursor must point to a root"

let gc ~src:_ _ ~dest:_ = failwith "not implemented"

let open_context ~filename:_ = failwith "not implemented"

let hash = NodeHash.hash
