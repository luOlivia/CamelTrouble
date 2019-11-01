type cell =
  | Wall
  | Movable
  | Empty 

exception InvalidMazeChar of string

let maze_HEIGHT = 40 
let maze_WIDTH = 80 

type t = cell array array

(** [empty_maze] is the empty maze filled with type Empty*)
let empty_maze = Array.make_matrix maze_WIDTH maze_HEIGHT Empty 

(** [cell_of_str c] is the cell type corresponding to string char [c]*)
let cell_of_str c = 
  match c with 
  | "#" -> Wall
  | "." -> Movable
  | "^" -> Empty
  | a -> raise (InvalidMazeChar a)

let str_to_cell s = 
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
  Array.set maze idx (Array.of_list cell_lst); maze

(** [read_lines text maze idx] is the maze that has been filled with each line 
    in text representing a row.*)
let rec read_lines text maze idx = 
  try 
    read_lines text (read (input_line text) maze idx) (idx-1)
  with End_of_file -> close_in text; maze 

(** [parse_map_file] is the Maze.t type parsed from the map file of [file_path]
    Requires: Text file that has 80 characters in each row, and 40 lines, with each 
    character either being the # symbol, ^ symbol, or . symbol.*)
let maze_from_file file_path =
  let raw_text = open_in file_path in 
  read_lines raw_text empty_maze 0

(**[print_maze maze] prints in terminal a textual representation of the maze*)
let print_maze maze = 
  let print_maze_row row = 
    row 
    |> Array.to_list 
    |> List.map (fun x -> (str_to_cell x)) 
    |> String.concat "" 
    |> print_endline 
  in Array.map print_maze_row maze 