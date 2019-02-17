open Plebeia_impl

(* What follows is just for debugging purposes, to be removed. *)

let rec string_of_tree root indent =
  (* pretty prints a tree to a string *)
  let indent_string = String.concat "" (List.init indent (fun _ -> " . ")) in
  let Node node = root in
  match node with
    | (Disk (index, _)) -> Printf.sprintf "%sDisk %Ld" indent_string index
    | View (Leaf (value, _, _, _)) ->
      Printf.sprintf "%sLeaf %s\n" indent_string (Value.to_string value)
    | View (Bud  (node , _, _, _)) ->
      let recursive =
        match node with
        | Some node -> (string_of_tree (Node node) (indent + 1))
        | None     ->  "Empty"
      in
      Printf.sprintf "%sBud:\n%s" indent_string recursive
    | View (Internal (left, right, _, _, _)) ->
      Printf.sprintf "%sInternal:\n%s%s" indent_string
        (string_of_tree (Node left) (indent + 1))
        (string_of_tree (Node right) (indent + 1))
    | View (Extender (segment, node, _, _, _)) ->
      Printf.sprintf "%s[%s]- %s" indent_string (Path.to_string segment)
        (string_of_tree (Node node) (indent + 1))

(* Graphviz's dot file format *)
let dot_of_tree root =
  let rec aux cntr (Node node) =
    match node with
    | Disk (index, _) -> 
        let n = Printf.sprintf "Disk%Ld" index in
        (n, [Printf.sprintf "%s [shape=box];" n], cntr)
    | View (Leaf (value, _, _, _)) ->
        let n = Printf.sprintf "Leaf%d\n" cntr in
        (n, [Printf.sprintf "%s [label=%S];" n (Value.to_string value)], cntr+1)
    | View (Bud  (Some node , _, _, _)) ->
        let n', s, cntr = aux cntr (Node node) in
        let n = Printf.sprintf "Bud%d" cntr in
        (n, 
         [Printf.sprintf "%s [shape=diamond, label=\"\"];" n;
          Printf.sprintf "%s -> %s;" n n'
         ] @ s,
         cntr + 1)
    | View (Bud  (None , _, _, _)) ->
        let n = Printf.sprintf "Bud%d" cntr in
        (n, 
         [Printf.sprintf "%s [shape=diamond, label=\"\"];" n], 
         cntr + 1)
    | View (Internal (left, right, _, _, _)) ->
        let ln, ls, cntr = aux cntr (Node left) in 
        let rn, rs, cntr = aux cntr (Node right) in 
        let n = Printf.sprintf "Internal%d" cntr in
        (n,
         [ Printf.sprintf "%s [shape=circle, label=\"\"];" n;
           Printf.sprintf "%s -> %s [label=\"L\"];" n ln;
           Printf.sprintf "%s -> %s [label=\"R\"];" n rn ]
         @ ls @ rs,
         cntr + 1)
    | View (Extender (segment, node, _, _, _)) ->
        let n', s, cntr = aux cntr (Node node) in
        let n = Printf.sprintf "Extender%d" cntr in
        (n,
         Printf.sprintf "%s [shape=circle, label=\"\"];" n
         :: Printf.sprintf "%s -> %s [label=%S];" n n' (Path.to_string segment)
         :: s,
         cntr + 1)
  in
  let (_, s, _) = aux 0 root in
  "digraph G {\n" ^ String.concat "\n" s ^ "\n}\n"
  
