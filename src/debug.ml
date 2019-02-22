open Plebeia_impl

(* What follows is just for debugging purposes, to be removed. *)

open Error

let rec string_of_node : type a b c. (a, b, c) node -> int -> string = fun node indent ->
  (* pretty prints a tree to a string *)
  let indent_string = String.concat "" (List.init indent (fun _ -> " . ")) in
  match node with
    | (Disk (index, _)) -> Printf.sprintf "%sDisk %Ld" indent_string index
    | View (Leaf (value, _, _, _)) ->
      Printf.sprintf "%sLeaf %s\n" indent_string (Value.to_string value)
    | View (Bud  (node , _, _, _)) ->
      let recursive =
        match node with
        | Some node -> string_of_node node (indent + 1)
        | None     ->  "Empty"
      in
      Printf.sprintf "%sBud:\n%s" indent_string recursive
    | View (Internal (left, right, _, _, _)) ->
      Printf.sprintf "%sInternal:\n%s%s" indent_string
        (string_of_node left (indent + 1))
        (string_of_node right (indent + 1))
    | View (Extender (segment, node, _, _, _)) ->
      Printf.sprintf "%s[%s]- %s" indent_string (Path.to_string segment)
        (string_of_node node (indent + 1))

(* Graphviz's dot file format *)
let dot_of_node root =
  let rec aux : type a b c. int -> (a, b, c) node -> (string * string list * int) = fun cntr -> function
    | Disk (index, _) -> 
        let n = Printf.sprintf "Disk%Ld" index in
        (n, [Printf.sprintf "%s [shape=box];" n], cntr)
    | View (Leaf (value, _, _, _)) ->
        let n = Printf.sprintf "Leaf%d\n" cntr in
        (n, [Printf.sprintf "%s [label=%S];" n (Value.to_string value)], cntr+1)
    | View (Bud  (Some node , _, _, _)) ->
        let n', s, cntr = aux cntr node in
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
        let ln, ls, cntr = aux cntr left in 
        let rn, rs, cntr = aux cntr right in 
        let n = Printf.sprintf "Internal%d" cntr in
        (n,
         [ Printf.sprintf "%s [shape=circle, label=\"\"];" n;
           Printf.sprintf "%s -> %s [label=\"L\"];" n ln;
           Printf.sprintf "%s -> %s [label=\"R\"];" n rn ]
         @ ls @ rs,
         cntr + 1)
    | View (Extender (segment, node, _, _, _)) ->
        let n', s, cntr = aux cntr node in
        let n = Printf.sprintf "Extender%d" cntr in
        (n,
         Printf.sprintf "%s [shape=circle, label=\"\"];" n
         :: Printf.sprintf "%s -> %s [label=%S];" n n' (Path.to_string segment)
         :: s,
         cntr + 1)
  in
  let (_, s, _) = aux 0 root in
  "digraph G {\n" ^ String.concat "\n" s ^ "\n}\n"

(* Graphviz's dot file format *)
let dot_of_cursor c = 
  go_top c >>= function Cursor (_, n, _) -> return @@ dot_of_node n

(* Bud -> Leaf and Bud -> Bud are invalid, but not excluded by the GADT *)
let validate_node (type a b c) context (node : (a, b, c) node) =
  let rec aux 
    : type a b c . (a, b, c) node -> ((a, b, c) view, string) result = 
    (* XXX Todo: No check of indexed_implies_hashed *)
    fun node ->
      let indexing_rule : type a b c . (a,b,c) view -> bool = function
        | Internal (_, _, Indexed _, _, _) -> true
        | Bud (_, Indexed _, _, _) -> true
        | Leaf (_, Indexed _, _, _) -> true
        | Extender (_, _, Indexed _, _, _) -> true
        | _ -> false
      in
      let hashed_is_transitive : type a b c . (a,b,c) view -> bool = function
        | Internal (_, _, _, Hashed _, _) -> true
        | Bud (_, _, Hashed _, _) -> true
        | Leaf (_, _, Hashed _, _) -> true
        | Extender (_, _, _, Hashed _, _) -> true
        | _ -> false
      in
      match node with
      | Disk  (i, wit) -> aux @@ View (load_node context i wit) 
      | View v -> 
          match v with
          | Leaf _ -> Ok v
          | Bud (None, _, _, _) -> Ok v
          | Bud (Some child, ir, hit, _iih) ->
              begin
                aux child >>= function
                | Bud _ -> Error "Bud cannot carry Bud"
                | Leaf _ -> Error "Bud cannot carry Leaf"
                | v' ->
                    (match ir, indexing_rule v' with
                     | _, true -> Ok ()
                     | Not_Indexed, _ -> Ok ()
                     | _ -> Error "Bud: Strange indexing_rule") >>= fun () ->
                    (match hit, hashed_is_transitive v' with
                     | _, true -> Ok v
                     | Not_Hashed, _ -> Ok v
                     | _ -> Error "Bud: Strange hashed_is_transitive")
              end
          | Internal (l, r, ir, hit, _iih) ->
              begin
                aux l >>= fun l ->
                aux r >>= fun r -> 
                (match ir, indexing_rule l, indexing_rule r with
                 | _, true, true -> Ok ()
                 | Left_Not_Indexed, false, _ -> Ok ()
                 | Right_Not_Indexed, _, false -> Ok ()
                 | Not_Indexed, _, _ -> Ok ()
                 | Indexed _, _, _ -> Error "Internal: Strange indexing_rule"
                 | Right_Not_Indexed, _, _ -> Error "Internal: Strange indexing_rule"
                 | Left_Not_Indexed, _, _ -> Error "Internal: Strange indexing_rule") >>= fun () ->
                (match hit, hashed_is_transitive l, hashed_is_transitive r with
                 | _, true, true -> Ok v
                 | Not_Hashed, _, _ -> Ok v
                 | _ -> Error "Internal: Strange hashed_is_transitive")
              end
          | Extender (seg, node, ir, hit, _) ->
              aux node >>= function
              | Extender _ -> Error "Extender cannot carry Extender"
              | v' ->
                  if List.length (seg :> Path.side list) > 223 then 
                    Error "segment too long"
                  else 
                    (match ir, indexing_rule v' with
                     | _, true -> Ok ()
                     | Not_Indexed, _ -> Ok ()
                     | _ -> Error "Extender: Strange hashed_is_transitive") >>= fun () ->
                    (match hit, hashed_is_transitive v' with
                     | _, true -> Ok v
                     | Not_Hashed, _ -> Ok v
                     | _ -> Error "Extender: Strange hashed_is_transitive")
  in
  aux node >>= function
  | Bud _ -> Ok ()
  | _ -> Error "Tree must start with a Bud"
