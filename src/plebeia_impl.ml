(** Implementation of space-efficient binary Patricia trees in OCaml.
    The implementation is geared for used in Tezos, though it is rather
    generic. A stop-and-copy GC is provided. This implementation aims
    to maximize correctness and cares second about efficiency. Extracting
    and efficient C program from F* should be explored. *)

type error = Types.error
module Index = Types.Index
module Value = Value
module Segment = Segment
module Hash = Hash
module Context = Context
module NodeHash = NodeHash
module Storage = Storage
module Cursor = Cursor
module Roots = Roots
module KVS = KVS
module Segment_encoding = Segment_encoding
module Utils = Utils
module Error = Error
module Key = Key
module Types = Types

include Node

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
      let c, i, h = commit c in
      Ok (c, i, h)
  | _ -> failwith "commit: cursor must point to the root"

let gc ~src:_ _ ~dest:_ = failwith "not implemented"

let hash = NodeHash.hash

let open_ = Context.open_
let close = Context.close
