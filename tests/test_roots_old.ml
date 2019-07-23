open Plebeia.Impl
open Roots_old
open Test_utils
open Stdint
    
let test_with_roots f =
  with_temp_file ~postfix:"root" f

module RS = Random.State
              
let random_hash st =
  Hash.of_string @@ random_string st 28
  
let random_index st =
  Uint32.of_int64 @@ RS.int64 st @@ Int64.(of_uint32 Uint32.max_int - 256L + 1L)

let st = Random.State.make_self_init ()

let () = test_with_roots @@ fun fn ->
  let t = create fn in
  let h = random_hash st in
  let i = random_index st in
  add t h i;
  close t;
  let t = open_ fn in
  begin match find t h with
    | None -> assert false
    | Some (i', _) when i <> i' -> 
        Format.eprintf "%Ld %Ld@." (Index.to_int64 i) (Index.to_int64 i');
        assert false
    | _ -> ()
  end;
  close t
