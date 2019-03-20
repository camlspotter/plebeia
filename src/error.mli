type ('a, 'b) t = ('a, 'b) result

val return : 'a -> ('a, 'b) result
(** monadic return *)

val (>>=) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** monadic bind *)

val (>>|) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
(** monadic map *)

val from_Ok : ('a, _) result -> 'a
(** It raises [Failure _] when the argument is [Error _]. *)

val from_Error : (_, 'e) result -> 'e
(** It raises [Failure _] when the argument is [Ok _]. *)

val default : (unit, 'e) result -> ('e -> unit) -> unit
  
val protect : (unit -> 'a) -> ('a, exn) result

val errorf : ('a, unit, string, ('b, string) result) format4 -> 'a
