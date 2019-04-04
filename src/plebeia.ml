include (Plebeia_impl : Plebeia_intf.S)

module Plebeia_impl = Plebeia_impl
(* Accessible to the lower level *)

module Path = Path
module Hash = Hash
module Value = Value
module KVS = KVS
module Segment_encoding = Segment_encoding
module Utils = Utils

module Error = Error
module Exn = Exn

module Debug = Debug

