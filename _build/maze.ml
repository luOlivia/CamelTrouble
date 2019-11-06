open Cell

let num_grid_squares = 7

(** [wall] of booleans representing the presence of walls. 
    A [cell x y] can have walls:
    - above [Horizontal (y)(x)]
    - below [Horizontal (y+1)(x)]
    - left  [Vertical (x)(y)]
    - below [Vertical (x+1)(y)]
*)
type wall = Horizontal of bool array array | Vertical of bool array array

type t = {horizontal_walls: wall; vertical_walls: wall}

let make_wall ()
  = Array.make_matrix (num_grid_squares+1) (num_grid_squares+1) false

let make_empty_maze () = 
  let maze = {
    horizontal_walls = Horizontal (make_wall ());
    vertical_walls = Vertical (make_wall ())
  } in
  for i = 0 to num_grid_squares do 
    (match maze.horizontal_walls with
     | Horizontal walls -> walls.(0).(i) <- true; walls.(num_grid_squares).(i) <- true
     | _ -> failwith "can only match horizontal wall");
    match maze.vertical_walls with
    | Vertical walls -> walls.(0).(i) <- true; walls.(num_grid_squares).(i) <- true
    | _ -> failwith "can only match vertical wall" 
  done; maze

let is_wall_left maze x y = 
  match maze.vertical_walls with 
  | Vertical walls -> walls.(x).(y)
  | _ -> failwith "can only match vertical wall"

let is_wall_right maze x y = 
  match maze.vertical_walls with 
  | Vertical walls -> walls.(x+1).(y)
  | _ -> failwith "can only match vertical wall"

let is_wall_above maze x y = 
  match maze.horizontal_walls with 
  | Horizontal walls -> walls.(y).(x)
  | _ -> failwith "can only match horizontal wall"

let is_wall_below maze x y =
  match maze.horizontal_walls with 
  | Horizontal walls -> walls.(y+1).(x)
  | _ -> failwith "can only match horizontal wall"

let neighbors maze cell = 
  let x = cell.x in
  let y = cell.y in 
  let nbrs = [cell] in
  let nbrs' = if not (is_wall_left maze x y) 
    then ((make_cell (x-1) y)::nbrs)
    else nbrs in 
  let nbrs'' = if not (is_wall_right maze x y) 
    then ((make_cell (x+1) y)::nbrs')
    else nbrs' in
  let nbrs''' = if not (is_wall_above maze x y) 
    then ((make_cell x (y-1))::nbrs'')
    else nbrs'' in
  let nbrs'''' = if not (is_wall_below maze x y) 
    then ((make_cell x (y+1))::nbrs''')
    else nbrs'''
  in nbrs''''

let is_neighbors cell1 cell2 = 
  let x1 = cell1.x in
  let y1 = cell1.y in 
  let x2 = cell2.x in 
  let y2 = cell2.y in 
  (x2-1=x1 && y2=y1)
  || (x2+1=x1 && y2=y1)
  || (x2=x1 && y2-1=y1)
  || (x2=x1 && y2+1=y1)

let walled_neighbors maze cell = 
  let x = cell.x in
  let y = cell.y in 
  let nbrs = [] in
  let nbrs' = if (is_wall_left maze x y) && x-1 >= 0 
    then ((make_cell (x-1) y)::nbrs)
    else nbrs in 
  let nbrs'' = if (is_wall_right maze x y) && x+1 < num_grid_squares
    then ((make_cell (x+1) y)::nbrs')
    else nbrs' in
  let nbrs''' = if not (is_wall_above maze x y) && y-1 >= 0 
    then ((make_cell x (y-1))::nbrs'')
    else nbrs'' in
  let nbrs'''' = if not (is_wall_below maze x y) && y+1 < num_grid_squares
    then ((make_cell x (y+1))::nbrs''')
    else nbrs'''
  in nbrs''''

let union cells1 cells2 = 
  cells1 @ cells2 |> List.sort_uniq Stdlib.compare

let intersect cells1 cells2 =
  List.fold_left 
    (fun a x -> 
       if (List.exists (fun y -> y = x) cells1) 
       then x::a 
       else a) 
    [] cells2

let area_neighbors maze area = 
  List.map (fun x -> walled_neighbors maze x) area 
  |> List.fold_left union []

let remove_wall maze area =
  let neighbors = area_neighbors maze area in
  for i = 0 to 1 do
    let chosen = neighbors |> List.length |> Random.int |> List.nth neighbors in
    let x1, y1 = chosen.x, chosen.y in
    let x2, y2 = 
      (match List.find_opt (fun x -> is_neighbors chosen x) area with 
       | None -> 0, 0
       | Some cell -> cell.x, cell.y) in
    if x1 = x2 then 
      match maze.horizontal_walls with 
      | Horizontal walls -> walls.(max y1 y2).(x1) <- false
      | _ -> failwith "can only match horizontal wall"
    else
      match maze.vertical_walls with 
      | Vertical walls -> walls.(max x1 x2).(y1) <- false
      | _ -> failwith "can only match vertical wall"
  done; maze

let make_maze density = 
  let maze = make_empty_maze () in 
  for i = 0 to density - 1 do
    let rand1 = Random.int (num_grid_squares + 1) in 
    let rand2 = Random.int (num_grid_squares + 1) in 
    match maze.horizontal_walls with 
    | Horizontal walls -> walls.(rand1).(rand2) <- true
    | _ -> failwith "can only match horizontal wall"
  done;
  for i = 0 to density - 1 do
    let rand1 = Random.int (num_grid_squares + 1) in 
    let rand2 = Random.int (num_grid_squares + 1) in 
    match maze.vertical_walls with 
    | Vertical walls -> walls.(rand1).(rand2) <- true
    | _ -> failwith "can only match vertical wall"
  done; maze

let connections maze = 
  let connctns = Array.make_matrix (num_grid_squares * num_grid_squares) 5 in
  connctns