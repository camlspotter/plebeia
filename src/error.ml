(* "Error" monad *)

type ('a, 'b) t = ('a, 'b) result

let return x = Ok x

let (>>|) y f = match y with
  | Ok x -> Ok (f x)
  | Error e -> Error e
(* Error monad operator. *)

let (>>=) x f = match x with
  | Error e -> Error e
  | Ok x -> f x

let from_Ok = function
  | Ok x -> x
  | Error _ -> failwith "Expected an Ok"

let from_Error = function
  | Ok _ -> failwith "Expected an Error"
  | Error e -> e
    
let default r f = match r with
  | Ok x -> x
  | Error e -> f e

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt
