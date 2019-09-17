val commit_top_cursor : Cursor.t -> Cursor.t * Index.t * Hash.t
(** Commit the node pointed by the cursor to the storage.
    It returns the updated cursor with the index and the hash
    of the stored node.
*)

val load_fully : Cursor.t -> Cursor.t
(** Recursively load the node pointed by the cursor. *)
