module Int64 = Stdint.Int64
  
(* XXX Problem in 32bit arch
     
   * [Cstruct.of_bigarray] only takes [char] indexed [Bigstring.t].
   * Offset must be [int] in [Cstruct].
   
   The current simple implementation to map the entire file to one [Bigstring.t]
   restricts the maximum file size in 32bit arch to [1_073_741_823], which is roughly just 1GB.
*)

type t = {
  storage : Storage.t ;

  hashcons : Hashcons.t ;
  (* Hashcons tbl *)

  stat : Stat.t ;
  (* Statistics *)
}

let create ?(pos=0L) ?length fn =
  let storage = Storage.create ~pos ?length fn in
  let hashcons = Hashcons.create storage in
  { storage ;
    hashcons ;
    stat = Stat.create ()
  }

let ref_load_leaf_value : (t -> Index.t -> Value.t option) ref = ref (fun _ -> assert false)

let open_ ?(pos=0L) ?(shared=false) fn =
  let storage = Storage.open_ ~pos ~shared fn in
  let hashcons = Hashcons.create storage in
  let t = { storage ;
            hashcons ;
            stat = Stat.create () 
          }
  in
  Hashcons.read t.hashcons  ~load_leaf_value:(!ref_load_leaf_value t);
  t

let close { storage ; _ } = Storage.close storage
