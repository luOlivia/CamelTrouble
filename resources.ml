let size = 20

(* colors *)
let sand = Graphics_js.rgb 252 213 145
let wall_color = Graphics_js.rgb 69 47 67

let purple1 = Graphics_js.rgb 29 3 56
let purple2 = Graphics_js.rgb 196 135 139
let purple3 = Graphics_js.rgb 219 147 152(*132 84 110*)
let purple_grad = [purple3; purple2; purple1; purple2; purple3]

let blue1 = Graphics_js.rgb 6 10 105
let blue2 = Graphics_js.rgb 23 27 145
let blue3 = Graphics_js.rgb 99 102 214
let blue4 = Graphics_js.rgb 187 189 252
let blue_grad = [blue3; blue2; blue1; blue2; blue3]

let red1 = Graphics_js.rgb 115 5 14
let red2 = Graphics_js.rgb 179 30 43
let red3 = Graphics_js.rgb 212 63 76
let red4 = Graphics_js.rgb 255 150 159
let red_grad = [red3; red2; red1; red2; red3]

(** [gradient_text color xpos ypos str gap] prints [str] for each color in [color] *)
let gradient_text color xpos ypos str gap = 
  List.fold_left (fun acc color -> begin
        Graphics_js.set_color color;
        Graphics.moveto xpos acc; 
        Graphics.draw_string str;
        acc - gap
      end) ypos color

let draw_string color s x y str =
  Graphics.set_text_size s;
  Graphics_js.set_color color;
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



(* let cameltext = [
   " $$$$$$\\        $$$$$$\\       $$\\      $$\\      $$$$$$$$\\      $$\\       ";
   "$$  __$$\\      $$  __$$\\      $$$\\    $$$ |     $$  _____|$     $ |      ";
   "$$ /  \\__|     $$ /  $$ |$     $$$\\  $$$$ |$     $ |      $$      |      ";
   "$$ |      $     $$$$$$$ |$$     \\$$\\$$ $$ |$     $$$$\\    $     $ |      ";
   "$$ |      $     $  __$$ |$$      \\$$$  $$ |$$       __|   $$      |      ";
   "$$ |  $$\\      $$ |  $$ |$     $ |\\$  /$$ |$     $ |      $$      |      ";
   "\\$$$$$$  |     $$ |  $$ |$     $ | \\_/ $$ |$     $$$$$$$\\ $     $$$$$$$\\ ";
   " \\______/      \\__|  \\__     |\\__|     \\_     _|\\_______     _|\\________|"
   ] *)
(* let cameltext = [
   " ██████╗ █████╗ ███╗   ███╗███████╗██╗     ";
   "██╔════╝██╔══██╗████╗ ████║██╔════╝██║     ";
   "██║     ███████║██╔████╔██║█████╗  ██║     ";
   "██║     ██╔══██║██║╚██╔╝██║██╔══╝  ██║     ";
   "╚██████╗██║  ██║██║ ╚═╝ ██║███████╗███████╗";
   " ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝"
   ] *)
(* let cameltext = [
   "   ___      .    __   __ .____  .    ";
   " .\'   \\    /|    |    |  /      /    ";
   " |        /  \\   |\\  /|  |__.   |    ";
   " |       /---\'\\  | \\/ |  |      |    ";
   " `.__,,\'      \\ /    /  /----/ /---/";
   ] *)
(* let cameltext = [
   "..1ad8888ba1...........db...........88b...........d88....88888888888....88...........";
   ".d877....778b.........d88b..........888b.........d888....88.............88...........";
   "d87..................d8778b.........8878b.......d8788....88.............88...........";
   "88..................d87..78b........88.78b.....d87.88....88aaaaa........88...........";
   "88.................d8YaaaaY8b.......88..78b...d87..88....88777777.......88...........";
   "Y81...............d8777777778b......88...78b.d87...88....88.............88...........";
   ".Y8a1....1a8P....d87........78b.....88....78887....88....88.............88...........";
   "..77Y8888Y77....d87..........78b....88.....787.....88....88888888888....88888888888.."
   ]
   let troubletext = [
   "_____  _     _         _         __";
   "  |   |_)  / \\|   |  |_)|    | |_ ";
   "  |   | \\  \\_/  |_  ||_)  |__ |__"
   ] *)
(* let troubletext = [
   "  _______ .___    ___   .     . ____   .     .____ ";
   " \'   /    /   \\ .\'   `. /     / /   \\  /     /     ";
   "     |    |__-\' |     | |     | |,_-<  |     |__.  ";
   "     |    |  \\  |     | |     | |    ` |     |     ";
   "     /    /   \\  `.__.\'  `._.\'  `----\' /---/ /----/"
   ] *)
(* let troubletext = [
   "$$$$$$$$\\ $$$$$$$\\   $$$$$$\\  $$\\   $$\\ $$$$$$$\\  $$\\       $$$$$$$$\\ ";
   "\\__$$  __|$$  __$$\\ $$  __$$\\ $$ |  $$ |$$  __$$\\ $$ |      $$  _____|";
   "   $$ |   $$ |  $$ |$$ /  $$ |$$ |  $$ |$$ |  $$ |$$ |      $$ |      ";
   "   $$ |   $$$$$$$  |$$ |  $$ |$$ |  $$ |$$$$$$$\\ |$$ |      $$$$$\\    ";
   "   $$ |   $$  __$$< $$ |  $$ |$$ |  $$ |$$  __$$\\ $$ |      $$  __|   ";
   "   $$ |   $$ |  $$ |$$ |  $$ |$$ |  $$ |$$ |  $$ |$$ |      $$ |      ";
   "   $$ |   $$ |  $$ | $$$$$$  |\\$$$$$$  |$$$$$$$  |$$$| $$$$$$$$\\ ";
   "   \\__|   \\__|  \\__| \\______/  \\______/ \\_______/ \\__|\\________|"
   ] *)
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

let rec draw_ascii word x y = 
  match word with
  | [] -> ()
  | h::t -> Graphics_js.moveto x y; Graphics_js.draw_string h; draw_ascii t x (y-size)

let draw word x y = 
  print_endline "fuckign draw";
  Graphics.set_text_size size;
  if word = "player1" then draw_ascii player1 x y
  else if word = "player2" then draw_ascii player2 x y
  else draw_ascii desert x y