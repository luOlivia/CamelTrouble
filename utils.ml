(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let cosine degree = degree |> to_radians |> Stdlib.cos

let sine degree = degree |> to_radians |> Stdlib.sin

(** [print_list lst] is [lst] printed *)
let rec print_list = function 
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let truncate x = 
  x 
  |> int_of_float
  |> float_of_int

let difference l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1