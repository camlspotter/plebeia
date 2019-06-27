include (Plebeia_impl : Plebeia_intf.S)
(* Standard library, where the interfaces are restricted. *)

module Plebeia_impl = Plebeia_impl
(* Unrestricted version, for library development and tests. *)

module Debug = Debug
