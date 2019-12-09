let size = 20

(* maze colors *)
let sand = Graphics_js.rgb 252 213 145
let wall_color = Graphics_js.rgb 69 47 67
let camel_c1 = Graphics_js.rgb 173 123 5
let camel_c1_hump = Graphics_js.rgb 148 105 4
let camel_c2 = Graphics_js.rgb 89 72 30
let camel_c2_hump = Graphics_js.rgb 56 44 17

(* shades of purple *)
let purple1 = Graphics_js.rgb 29 3 56
let purple2 = Graphics_js.rgb 196 135 139
let purple3 = Graphics_js.rgb 219 147 152
let purple_grad = [purple3; purple2; purple1; purple2; purple3]

(* shades of blue *)
let blue1 = Graphics_js.rgb 6 10 105
let blue2 = Graphics_js.rgb 23 27 145
let blue3 = Graphics_js.rgb 99 102 214
let blue4 = Graphics_js.rgb 187 189 252
let blue_grad = [blue3; blue2; blue1; blue2; blue3]

(* shades of red *)
let red1 = Graphics_js.rgb 115 5 14
let red2 = Graphics_js.rgb 179 30 43
let red3 = Graphics_js.rgb 212 63 76
let red4 = Graphics_js.rgb 255 150 159
let red_grad = [red3; red2; red1; red2; red3]


let gradient_text color xpos ypos str gap = 
  List.fold_left (fun acc color -> begin
        Graphics_js.set_color color; Graphics.moveto xpos acc; 
        Graphics.draw_string str; acc - gap
      end) ypos color

let draw_string c s x y str =
  Graphics.set_text_size s;
  Graphics_js.set_color c;
  Graphics.moveto x y; 
  Graphics.draw_string str

let desert = [
  "                                           =--_";
  "                            .-\"\"\"\"-.     |* _)";
  "                           /          \\    /  /";
  "                          /            \\_/  /";
  "                         /|                /";
  "\/\\                       / |    ____    _.-\" ";
  " \'-_            _       (   \\  |\\  /\\  ||    ";
  "'   `\'-._   .-\'\"/'.      \"   | |/ /  | |/    ";
  " '.      _-\"   (   \'-_       \\ | /   \\ |     ";
  ".'   _.-\'       )     \"-._    ||\\\\   |\\\\ ";
  "             .-\'          `\'  || \\\\  ||))";
  "_ _ ____________ _____  ___ _|\\ _|\\_|\\\\/";
  "          c  c  \" c C \"\"C  \" \"\"  \"\" \"\""
]

(* player controls *)
let player1 = [
  "PLAYER ONE";
  "        _   _   ";
  "       /  \\ /  \\ ";
  "      | W | E |  ";
  "       \\_/  \\_/";
  "   _   _   _   ";
  "  /  \\ /  \\ /  \\ ";
  " | A | S | D | ";
  "  \\_/ \\_/ \\_/ "
]
let player2 = [
  "PLAYER TWO";
  "        _   _   ";
  "       /  \\ /  \\ ";
  "      | ⦁ |  ↑ |  ";
  "       \\_/ \\_/";
  "   _   _   _   ";
  "  /  \\ /  \\ /  \\ ";
  " |← | ↓ |→| ";
  "  \\_/ \\_/ \\_/ "
]

(** [draw_ascii word x y] draws lines in [word] in successive pos [x][y] *)
let draw_ascii word x y = 
  ignore (List.fold_left (fun acc s -> begin
        Graphics_js.moveto x acc; Graphics_js.draw_string s; (acc-size)
      end) y word)

let draw word x y = 
  Graphics.set_text_size size;
  if word = "player1" then draw_ascii player1 x y
  else if word = "player2" then draw_ascii player2 x y
  else draw_ascii desert x y

let audio name () =
  let elt = Js_of_ocaml.Dom_html.createAudio Js_of_ocaml.Dom_html.document in
  elt##.src := (Js_of_ocaml.Js.string ("sounds/" ^ name ^ ".wav"));
  elt##play

let getElementById coerce id =
  match Js_of_ocaml.Js.Opt.to_option @@ Js_of_ocaml.Dom_html.document##getElementById (Js_of_ocaml.Js.string id) with
  | None -> failwith id
  | Some x -> match Js_of_ocaml.Js.Opt.to_option @@ coerce x with
    | None -> failwith id
    | Some x -> x

let get_input_name (player: Camel.player_num) = 
  let elt_name = 
    match player with
    | One -> "p1name"
    | Two -> "p2name" in 
  let input_elt = getElementById Js_of_ocaml.Dom_html.CoerceTo.textarea elt_name in 
  Js_of_ocaml.Js.to_string input_elt##.value
