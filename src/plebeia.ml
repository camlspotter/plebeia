(* Implementation of space-efficient binary Patricia trees in OCaml.
   The implementation is geared for used in Tezos, though it is rather
   generic. A stop-and-copy GC is provided. This implementation aims
   to maximize correctness and cares second about efficiency. *)

(* Internal implementation is exposed in this module.  For testing and
   debugging.
*)
module Impl = struct
  
  (* Base *)
  module Error            = Error
  module Utils            = Utils
  module Result           = Result

  (* Core *)
  module Value            = Value
  module Index            = Index
  module Segment          = Segment
  module Context          = Context
  module Node             = Node
  module Cursor           = Cursor

  (* Hash *)
  module Hash             = Hash
  module Node_hash        = Node_hash
  module Cursor_hash      = Cursor_hash

  (* Storage *)
  module Segment_encoding = Segment_encoding
  module Storage          = Storage
  module Cursor_storage   = Cursor_storage

  (* High level *)
  module Deep             = Deep
  module Roots            = Roots
  module Vc               = Vc

  (* Helper *)
  module Stat             = Stat
  module Debug            = Debug
  module Hashcons         = Hashcons


  (* Experimental *)
  module Key              = Key
end 

include (Impl : Plebeia_intf.S)
(* Standard APIs, module interfaces are restricted. *)
