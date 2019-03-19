(** Key-value store for leaf data using reference counting
    for deletion. TODO provide persistence to disk. *)

(** Type of a key-value store. *)
type t

(** New, empty table *)
val make : unit -> t

(** Inserts a key in the table, or update the reference
    count if the key is already present. *)
val insert   : t -> Hash.hash28 -> Value.t -> unit

(** Gets a value from the table, returns None if the key
    is not present. *)
val get_opt  : t -> Hash.hash28 -> Value.t option

(** Decrements the reference counter of a key in the table.
    Deletes the key if the counter falls to 0. *)
val decr     : t -> Hash.hash28 -> unit
