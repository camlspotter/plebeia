type t = {
  mtree : Mtree.t ; 
  (* sparse Merkle tree *)

  roots : Roots.t ;
  (* Persisitent DB of root hashes to indices in the array. *)
}

let make ?pos ?shared ?kvs ?length fn =
  let mtree = Mtree.make ?pos ?shared ?kvs ?length fn in
  let roots = Roots.create (fn ^ "_roots") in
  { mtree ;
    roots
  }

let open_ ?pos ?shared ?kvs fn =
  if not @@ Sys.file_exists fn then make ?pos ?shared ?kvs fn 
  else
    let mtree = Mtree.open_ ?pos ?shared fn in
    let roots = Roots.open_ (fn ^ "_roots") in
    { mtree ; roots }
  
let close { mtree ; roots ; _ } = 
  Roots.close roots;
  Mtree.close mtree

