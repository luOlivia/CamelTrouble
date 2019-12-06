open Cell

let _ = Random.self_init ()
let num_grid_squares = 7
let density = 16

(** [wall] of booleans representing the presence of walls. 
    A [cell x y] can have walls:
    - above [Horizontal (y)(x)]
    - below [Horizontal (y+1)(x)]
    - left  [Vertical (x)(y)]
    - below [Vertical (x+1)(y)]
*)
type wall = 
  | Horizontal of bool array array 
  | Vertical of bool array array

type t = 
  {
    horizontal_walls: wall; 
    vertical_walls: wall
  }

let make_wall ()
  = Array.make_matrix (num_grid_squares+1) (num_grid_squares+1) false

let make_empty_walls () = 
  let maze = {
    horizontal_walls = Horizontal (make_wall ());
    vertical_walls = Vertical (make_wall ())
  } in
  for i = 0 to num_grid_squares do 
    begin
      match maze.horizontal_walls with
      | Horizontal walls -> 
        walls.(0).(i) <- true; walls.(num_grid_squares).(i) <- true
      | _ -> failwith "can only match horizontal wall"
    end;
    begin
      match maze.vertical_walls with
      | Vertical walls -> 
        walls.(0).(i) <- true; walls.(num_grid_squares).(i) <- true
      | _ -> failwith "can only match vertical wall" 
    end
  done; 
  maze

let clip x y = 
  let x' = 
    if x < 0 then 0 
    else if x > num_grid_squares then num_grid_squares 
    else x in
  let y' = 
    if y < 0 then 0 
    else if y > num_grid_squares then num_grid_squares 
    else y in
  (x', y')

let is_wall_left maze x y = 
  let x, y = clip x y in
  match maze.vertical_walls with 
  | Vertical walls -> walls.(x).(y)
  | _ -> failwith "can only match vertical wall"

let is_wall_right maze x y =
  match maze.vertical_walls with 
  | Vertical walls -> walls.(x+1).(y)
  | _ -> failwith "can only match vertical wall"

let is_wall_above maze x y = 
  let x, y = clip x y in
  match maze.horizontal_walls with 
  | Horizontal walls -> walls.(y).(x)
  | _ -> failwith "can only match horizontal wall"

let is_wall_below maze x y =
  let x, y = clip x y in
  match maze.horizontal_walls with 
  | Horizontal walls -> walls.(y+1).(x)
  | _ -> failwith "can only match horizontal wall"

let neighbors maze cell = 
  let x = cell.x in
  let y = cell.y in 
  let nbrs = [cell] in
  let nbrs' = 
    if not (is_wall_left maze x y) 
    then (make_cell (x-1) y)::nbrs
    else nbrs in 
  let nbrs'' = 
    if not (is_wall_right maze x y) 
    then (make_cell (x+1) y)::nbrs'
    else nbrs' in
  let nbrs''' = 
    if not (is_wall_above maze x y) 
    then (make_cell x (y-1))::nbrs''
    else nbrs'' in
  let nbrs'''' = 
    if not (is_wall_below maze x y) 
    then (make_cell x (y+1))::nbrs'''
    else nbrs'''
  in nbrs'''' |> Array.of_list

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
  let nbrs' = 
    if (is_wall_left maze x y) && x-1 >= 0 
    then (make_cell (x-1) y)::nbrs
    else nbrs in 
  let nbrs'' = 
    if (is_wall_right maze x y) && x+1 < num_grid_squares
    then (make_cell (x+1) y)::nbrs'
    else nbrs' in
  let nbrs''' = 
    if not (is_wall_above maze x y) && y-1 >= 0 
    then (make_cell x (y-1))::nbrs''
    else nbrs'' in
  let nbrs'''' = 
    if not (is_wall_below maze x y) && y+1 < num_grid_squares
    then (make_cell x (y+1))::nbrs'''
    else nbrs'''
  in nbrs'''' |> Array.of_list

let union cells1 cells2 = 
  Array.append cells1 cells2 
  |> Array.to_list 
  |> List.sort_uniq Stdlib.compare 
  |> Array.of_list

let intersect cells1 cells2 =
  let cells1 = Array.to_list cells1 in 
  let cells2 = Array.to_list cells2 in 
  let f a x = 
    if (List.exists (fun y -> y = x) cells1) 
    then x::a 
    else a in 
  List.fold_left f [] cells2 
  |> Array.of_list

let difference l1 l2 = 
  List.filter (fun x -> not (List.mem x l2)) l1

let area_neighbors maze area = 
  (area
   |> Array.map (fun x -> walled_neighbors maze x) 
   |> Array.fold_left union [||] 
   |> Array.to_list 
   |> difference) 
    (area |> Array.to_list) 
  |> Array.of_list

let choose_random neighbors = 
  neighbors 
  |> Array.length 
  |> Random.int 
  |> Array.get neighbors

let get_adjacent chosen area =
  let adjacent_cell = 
    List.find_opt (fun x -> is_neighbors chosen x) (Array.to_list area) in
  match adjacent_cell with 
  | None -> 0, 0
  | Some cell -> cell.x, cell.y 

let remove_wall maze area =
  let neighbors = area_neighbors maze area in
  for i = 0 to 1 do
    let chosen = choose_random neighbors in
    let x1, y1 = chosen.x, chosen.y in
    let x2, y2 = get_adjacent chosen area in
    if x1 = x2 then 
      match maze.horizontal_walls with 
      | Horizontal walls -> walls.(max y1 y2).(x1) <- false
      | _ -> failwith "can only match horizontal wall"
    else
      match maze.vertical_walls with 
      | Vertical walls -> walls.(max x1 x2).(y1) <- false
      | _ -> failwith "can only match vertical wall"
  done; 
  maze

let make_random_walls density = 
  let maze = make_empty_walls () in 
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

let get_connections maze = 
  let connections = Array.make (num_grid_squares * num_grid_squares) [||] in
  for i = 0 to num_grid_squares - 1 do
    for j = 0 to num_grid_squares - 1 do 
      connections.(i + j * num_grid_squares) <- neighbors maze {x=i; y=j}
    done
  done; connections

let extract_opt = function
  | Some y -> y
  | None -> failwith "cannot use on None"

let extract_connections connections_opt = 
  let len = 
    Array.fold_left (fun a x -> if x <> None then (a + 1) else a) 
      0 connections_opt in
  let res = 
    make_cell 0 0
    |> Array.make_matrix len (num_grid_squares * num_grid_squares) in
  let i = ref 0 in 
  let j = ref 0 in
  while !i < len && !j < Array.length connections_opt do
    if connections_opt.(!j) <> None then 
      begin
        res.(!i) <- extract_opt connections_opt.(!j); 
        incr i
      end;
    incr j;
  done; 
  res

let merge_all maze = 
  let init_connections = get_connections maze in
  let connections_opt = Array.map (fun x -> Some x) init_connections in
  let i = ref 0 in
  while !i < (Array.length connections_opt)-1 do
    let j = ref (!i+1) in
    while !j < (Array.length connections_opt) do
      match connections_opt.(!i), connections_opt.(!j) with
      | Some lst_i, Some lst_j -> 
        if intersect lst_i lst_j <> [||] then
          begin
            connections_opt.(!i) <- Some (union lst_i lst_j);
            connections_opt.(!j) <- None;
            i := 0;
            j := 0;
          end
        else 
          incr i; incr j
      | _ -> incr i; incr j;
    done
  done;
  extract_connections connections_opt

let make_maze density = 
  (* make_empty_walls () *)
  let walls = ref (make_random_walls density) in 
  let areas = ref (merge_all !walls) in
  for i = 0 to density - 1 do
    walls := remove_wall !walls !areas.(0);
    areas := merge_all !walls;
  done; 

  for i = 0 to num_grid_squares do 
    begin
      match !walls.horizontal_walls with
      | Horizontal walls -> 
        walls.(0).(i) <- true; walls.(num_grid_squares).(i) <- true
      | _ -> failwith "can only match horizontal wall";
    end;
    begin
      match !walls.vertical_walls with
      | Vertical walls -> 
        walls.(0).(i) <- true; walls.(num_grid_squares).(i) <- true
      | _ -> failwith "can only match vertical wall" 
    end
  done; 
  !walls

let to_str maze = 
  let lines = Array.make (num_grid_squares+1) "" in 
  lines.(0) <- " ";
  for i = 0 to num_grid_squares - 1 do
    match maze.horizontal_walls with
    | Horizontal walls -> if walls.(0).(i) then lines.(0) <- lines.(0) ^ "_ "
      else lines.(0) <- lines.(0) ^ "  "
    | _ -> failwith "can only match horizontal wall"
  done;
  for i = 1 to num_grid_squares do
    for j = 0 to num_grid_squares do
      begin 
        match maze.vertical_walls with
        | Vertical walls -> if walls.(j).(i-1) then lines.(i) <- lines.(i) ^ "|"
          else lines.(i) <- lines.(i) ^ " "
        | _ -> failwith "can only match vertical wall"
      end;
      begin
        match maze.horizontal_walls with
        | Horizontal walls -> if walls.(i).(j) then lines.(i) <- lines.(i) ^ "_"
          else lines.(i) <- lines.(i) ^ " "
        | _ -> failwith "can only match horizontal wall"
      end
    done
  done;
  lines 
  |> Array.to_list 
  |> String.concat "\n"

