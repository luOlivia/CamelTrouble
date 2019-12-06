(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let cosine degree = degree |> to_radians |> Stdlib.cos

let sine degree = degree |> to_radians |> Stdlib.sin