open Plebeia.Impl.Key

module RS = Random.State

let gen_char k st = match k with
  | Hex ->
      let x = RS.int st 16 in
      Char.chr 
      @@ if x < 10 then Char.code '0' + x
         else Char.code 'a' + x - 10
  | Lower ->
      let x = RS.int st 32 in
      if x < 26 then Char.chr @@ Char.code 'a' + x
      else begin 
        match x with
        | 26 -> ' '
        | 27 -> '-'
        | 28 -> '.'
        | 29 -> ':'
        | 30 -> '='
        | 31 -> '_'
        | _ -> assert false
      end
  | Alphanum ->
      let x = RS.int st 64 in
      if x < 10 then Char.chr @@ Char.code '0' + x
      else if x < 36 then Char.chr @@ Char.code 'A' + x - 10
      else if x < 62 then Char.chr @@ Char.code 'a' + x - 36
      else begin 
        match x with
        | 62 -> '-'
        | 63 -> '_'
        | _ -> assert false
      end
  | Ascii -> Char.chr @@ RS.int st 128
  | Binary -> Char.chr @@ RS.int st 256

let gen_string st =
  let ki = RS.int st 5 in
  let k = List.nth [Hex ; Lower ; Alphanum ; Ascii ; Binary ] ki in
  let len = RS.int st 256 + 1 in
  String.init len (fun _ -> gen_char k st)

let test s =
  match to_segments s with
  | Error s -> failwith s
  | Ok segs -> 
      match of_segments segs with
      | Ok s' when s <> s' ->
          Format.eprintf "ERROR %S => %S@." s s';
          assert false
      | Ok _ -> ()
      | Error e -> 
          Format.eprintf "ERROR %S => %s@." s e;
          assert false

let () =
  let st = Random.State.make_self_init () in
  for _ = 1 to 1000000 do test (gen_string st) done
  
