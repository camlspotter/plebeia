(** Result monad *)

type ('a, 'b) t = ('a, 'b) result

val return : 'a -> ('a, 'b) t
(** monadic return *)

val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
(** monadic bind *)

val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
val (>>|) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
(** monadic map *)

val from_Ok : ('a, _) t -> 'a
(** It raises [Failure _] when the argument is [Error _]. *)

val from_Error : (_, 'e) t -> 'e
(** It raises [Failure _] when the argument is [Ok _]. *)

val default : (unit, 'e) t -> ('e -> unit) -> unit
  
val errorf : ('a, unit, string, ('b, string) t) format4 -> 'a
