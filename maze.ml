type cell =
  | Wall
  | Movable
  | Empty 

exception InvalidMazeChar of string

let maze_HEIGHT = 10
let maze_WIDTH = 20 
let num_PLAYERS = 2

type maze = (cell array) array

type t = {spawn_points: (float*float) list ; maze: maze}

(** [empty_maze] is the empty maze filled with type Empty*)
let empty_maze = Array.make_matrix maze_HEIGHT maze_WIDTH Empty
let empty_maze_t = {
  spawn_points = [];
  maze = empty_maze;
}
let make_t lst m = {
  spawn_points = lst;
  maze = m;
}

let reverse_array array = 
  let len=Array.length array in
  for i=0 to len/2-1 do 
    let temp = array.(i) in
    array.(i) <- array.(len-i-1);
    array.(len-i-1) <- temp         
  done;
  array;;


(** [cell_of_str c] is the cell type corresponding to string char [c]*)
let cell_of_str c = 
  match c with 
  | "#" -> Wall
  | "." -> Movable
  | "^" -> Empty
  | a -> raise (InvalidMazeChar a)

let str_of_cell s = 
  match s with
  | Wall -> "#"
  | Movable -> "."
  | Empty -> "^"

(**[read line maze idx] takes a line, parses it into a list of cells, and then 
   instantiates [maze] at index [idx] with this list*) 
let read line maze idx = 
  let rec parse_line acc = function
    | [] -> acc 
    | h::t -> parse_line ((cell_of_str h)::acc) t in
  let cell_lst =  parse_line [] (Str.split (Str.regexp "") line) in
  Array.set maze idx (Array.of_list cell_lst); 
  maze


(** [read_lines text maze idx] is the maze that has been filled with each line 
    in text representing a row.*)
let rec read_lines text m idx = 
  try 
    if idx < maze_HEIGHT then
      read_lines text (read (input_line text) m idx) (idx+1)
    else m 
  with End_of_file -> close_in text; m 

(** [parse_map_file] is the Maze.t type parsed from the map file of [file_path]
    Requires: Text file that has 80 characters in each row, and 40 lines, with each 
    character either being the # symbol, ^ symbol, or . symbol.*)
let maze_from_file file_path =
  let raw_text = open_in file_path in 
  let parsed_maze = read_lines raw_text empty_maze 0 in 
  make_t [] (reverse_array parsed_maze)


(**[print_maze maze] prints in terminal a textual representation of the maze*)
let print_maze t = 
  let maze = t.maze in 
  let print_maze_row row = 
    row 
    |> Array.to_list 
    |> List.map (fun x -> (str_of_cell x)) 
    |> String.concat "" 
  in Array.map print_maze_row (Array.copy maze) 
     |> Array.fold_left (fun a x -> x^"\n"^a) "" 