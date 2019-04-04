val catch : ('a -> 'b) -> 'a -> ('b, [> `Exn of exn]) result
val protect : (unit -> 'a) -> (unit -> unit) -> 'a
