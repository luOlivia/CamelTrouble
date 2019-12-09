open Camel

(* [size] is standard text size *)
val size : int

(** [sand] is maze background color *)
val sand : Graphics_js.color
(** [wall_color] is maze wall color *)
val wall_color : Graphics_js.color
(** [camel_c1] is player 1 color *)
val camel_c1 : Graphics_js.color
(** [camel_c1_hump] is player 1 hump color *)
val camel_c1_hump : Graphics_js.color
(** [camel_c2] is player 2 color *)
val camel_c2 : Graphics_js.color
(** [camel_c1_hump] is player 2 hump color *)
val camel_c2_hump : Graphics_js.color

(** [blue1] is player1 winning color *)
val blue1 : Graphics_js.color
(** [red1] is player1 winning color *)
val red1 : Graphics_js.color
(** [purple_grad] is a purple gradient *)
val purple_grad : Graphics_js.color list
(** [red_grad] is a red gradient *)
val red_grad : Graphics_js.color list
(** [blue_grad] is a blue gradient *)
val blue_grad : Graphics_js.color list

(** [gradient_text color xpos ypos str gap] prints [str] 
    for each color in [color] at position ([xpos], [ypos])
    with line spacing [gap] *)
val gradient_text : Graphics_js.color list 
  -> int -> int -> string -> int -> int

(** [draw_string c s x y str] draws [str] with color [c]
    and text size [s] at pos [x][y]*)
val draw_string : Graphics_js.color -> int -> int -> int -> string -> unit

(** [desert] is camel ascii art *)
val desert : string list
(** [player1] is player1 controls ascii art *)
val player1 : string list
(** [player2] is player2 controls ascii art *)
val player2 : string list

(** [draw word x y] draws [word] at pos [x][y] *)
val draw : string -> int -> int -> unit

(**[audio name] plays audio from file of [name].wav*)
val audio : string -> unit -> unit

val get_input_name : player_num -> string
