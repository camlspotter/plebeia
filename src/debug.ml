open Utils
open Plebeia_impl
    
(* What follows is just for debugging purposes, to be removed. *)

open Error

let rec string_of_node : node -> int -> string = fun node indent ->
  (* pretty prints a tree to a string *)
  let indent_string = String.concat "" (List.init indent (fun _ -> " . ")) in
  match node with
  | (Disk (index, _)) -> Printf.sprintf "%sDisk %Ld" indent_string (Index.to_int64 index)
  | View (Leaf (value, Indexed i, Hashed h, _x)) ->
      Printf.sprintf "%sLeaf %S (%Ld, %s)\n" indent_string (Value.to_string value)
        (Index.to_int64 i) (to_hex @@ Hash.to_string h)
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

module Dot = struct
  (* Graphviz's dot file format *)
  
  let link ?label n1 n2 =
    match label with
    | None -> Printf.sprintf "%s -> %s;" n1 n2
    | Some l -> Printf.sprintf "%s -> %s [label=\"%s\"];" n1 n2 l

  let disk n = Printf.sprintf "%s [shape=box];" n
  let leaf n value = Printf.sprintf "%s [label=%S];" n (Value.to_string value)
  let bud n = Printf.sprintf "%s [shape=diamond, label=\"\"];" n
  let internal n = Printf.sprintf "%s [shape=circle, label=\"\"];" n
  let extender = internal

  let of_node_aux cntr root =
    let rec aux : int -> node -> (string * string list * int) = fun cntr -> function
      | Disk (index, _) -> 
          let n = Printf.sprintf "Disk%Ld" (Index.to_int64 index) in
          (n, [disk n], cntr)
      | View (Leaf (value, _, _, _)) ->
          let n = Printf.sprintf "Leaf%d\n" cntr in
          (n, [leaf n value], cntr+1)
      | View (Bud  (Some node , _, _, _)) ->
          let n', s, cntr = aux cntr node in
          let n = Printf.sprintf "Bud%d" cntr in
          (n, 
           [bud n;
            link n n'
           ] @ s,
           cntr + 1)
      | View (Bud  (None , _, _, _)) ->
          let n = Printf.sprintf "Bud%d" cntr in
          (n, 
           [bud n], 
           cntr + 1)
      | View (Internal (left, right, _, _, _)) ->
          let ln, ls, cntr = aux cntr left in 
          let rn, rs, cntr = aux cntr right in 
          let n = Printf.sprintf "Internal%d" cntr in
          (n,
           [ internal n;
             link n ln ~label:"L";
             link n rn ~label:"R" ]
           @ ls @ rs,
           cntr + 1)
      | View (Extender (segment, node, _, _, _)) ->
          let n', s, cntr = aux cntr node in
          let n = Printf.sprintf "Extender%d" cntr in
          (n,
           extender n
           :: link n n' ~label:(Path.to_string segment)
           :: s,
           cntr + 1)
    in
    aux cntr root
  
  let rec of_trail dst cntr = function
    | Top -> ([], cntr)
    | Left (trail, r, _, _) ->
        let n = Printf.sprintf "Internal%d" cntr in
        let cntr = cntr + 1 in
        let r, ss, cntr = of_node_aux cntr r in
        let (ss', cntr) = of_trail n cntr trail in
        ([ internal n;
           link n dst ~label:"L";
           link n r ~label:"R" ]
         @ ss @ ss',
         cntr)
    | Right (l, trail, _, _) ->
        let n = Printf.sprintf "Internal%d" cntr in
        let cntr = cntr + 1 in
        let l, ss, cntr = of_node_aux cntr l in
        let (ss', cntr) = of_trail n cntr trail in
        ([ internal n;
           link n l ~label:"L";
           link n dst ~label:"R" ]
         @ ss @ ss',
         cntr)
    | Budded (trail, _, _) ->
        let n = Printf.sprintf "Bud%d" cntr in
        let cntr = cntr + 1 in
        let (ss, cntr) = of_trail n cntr trail in
        ([ bud n;
           link n dst ]
         @ ss,
         cntr)
    | Extended (trail, segment, _, _) -> 
        let n = Printf.sprintf "Extender%d" cntr in
        let cntr = cntr + 1 in
        let (ss, cntr) = of_trail n cntr trail in
        ([ extender n;
           link n dst ~label:(Path.to_string segment) ]
         @ ss,
         cntr)
    
  let make_digraph ss = "digraph G {\n" ^ String.concat "\n" ss ^ "\n}\n"
  
  let of_node root =
    let (_name, ss, _cntr) = of_node_aux 0 root in
    make_digraph ss
  
  let of_cursor (Cursor (trail, node, _)) =
    let (n, ss, cntr) = of_node_aux 0 node in
    let ss', _ = of_trail n cntr trail in
    let s = Printf.sprintf "cursor [shape=point, label=\"\"]; cursor -> %s [style=bold];" n in
    make_digraph (s :: ss @ ss')
end

let dot_of_node = Dot.of_node
let dot_of_cursor = Dot.of_cursor

(* Bud -> Leaf and Bud -> Bud are invalid, but not excluded by the GADT *)
let validate_node context (node : node) =
  let rec aux : node -> (view, string) result = 
    (* XXX Todo: No check of indexed_implies_hashed *)
    fun node ->
      let indexing_rule : view -> bool = function
        | Internal (_, _, Indexed _, _, _) -> true
        | Bud (_, Indexed _, _, _) -> true
        | Leaf (_, Indexed _, _, _) -> true
        | Extender (_, _, Indexed _, _, _) -> true
        | _ -> false
      in
      let hashed_is_transitive : view -> bool = function
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
