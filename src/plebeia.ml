include (Plebeia_impl 
         : Plebeia_intf.S
         with type value = Value.t
          and type segment = Path.segment
          and type error = string
          and type hash = Hash.hash56
          and type index = Types.Index.t
        )

module Plebeia_impl = Plebeia_impl
(* Accessible to the lower level *)

module Index = Types.Index
module Path = Path
module Hash = Hash
module Value = Value
module KVS = KVS
module Segment_encoding = Segment_encoding
module Utils = Utils

module Error = Error

module Debug = Debug

module Roots = Roots
