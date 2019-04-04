let catch f a = match f a with
  | exception e -> Error (`Exn e)
  | x -> Ok x

let protect f fin = 
  match f () with
  | exception e -> fin (); raise e
  | x -> fin (); x
