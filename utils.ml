(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let cosine degree = degree |> to_radians |> Stdlib.cos

let sine degree = degree |> to_radians |> Stdlib.sin

let rec print_list = function 
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l
