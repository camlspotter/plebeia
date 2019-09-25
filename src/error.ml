type t = ..

let printers : (t -> string option) list ref = ref []

let register_printer p = printers := p :: !printers

let show t =
  let rec loop = function
    | [] -> "Error (no printer registered)"
    | f::fs ->
        match f t with
        | None -> loop fs
        | Some s -> s
  in
  loop !printers
  
let raise e = failwith (show e)
    
