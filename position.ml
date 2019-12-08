type t = {x: float; y: float}

let distance p1 p2 = 
  let square x = x *. x in 
  ((p1.x -. p2.x) |> square) +. ((p1.y -. p2.y) |> square) |> sqrt

let init nx ny = {
  x = nx;
  y = ny
}