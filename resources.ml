
let desert = [
  "                                                        =--_";
  "                                         .-\"\"\"\"\"\"-.     |* _)";
  "                                        /          \\   /  /";
  "                                       /            \\_/  /";
  "           _                          /|                /";
  "       _-\'\"/\\                        / |    ____    _.-\" ";
  "    _-\'   (  \'-_            _       (   \\  |\\  /\\  ||    ";
  "_.-\'       '.   `\'-._   .-\'\"/'.      \"   | |/ /  | |/    ";
  "             \'.      _-\"   (   \'-_       \\ | /   \\ |     ";
  "           _.\'   _.-\'       )     \"-._    ||\\\\   |\\\\ ";
  "         \'               .-\'          `\'  || \\\\  ||))";
  "_____  _  ___  _ ____________ _____  ___ _|\\ _|\\_|\\\\/";
  "                       c  c  \" c C \"\"C  \" \"\"  \"\" \"\""
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
let cameltext = [
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
]
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
let playtext = [
  "PRESS ANY KEY TO";
  "   _   _   _   _   _   !";
  "  / \\ / \\ / \\ / \\ / \\ !";
  " ( S | T | A | R | T )  !";
  "  \\_/ \\_/ \\_/ \\_/ \\_/ !"
]

let rec draw_ascii word x y = 
  match word with
  | [] -> ()
  | h::t -> Graphics_js.moveto x y; Graphics_js.draw_string h; draw_ascii t x (y-15)

let draw word x y = 
  print_endline "fuckign draw";
  if word = "camel" then draw_ascii cameltext x y
  else if word = "trouble" then draw_ascii troubletext x y
  else if word = "play" then draw_ascii playtext x y
  else draw_ascii desert x y

(* let main () = 
   print_endline "drawing in resources";
   draw "desert" 10 300

   let _ =  print_endline "starting up";
   Js_of_ocaml.Js.Opt.iter
    (Js_of_ocaml.Dom_html.CoerceTo.canvas (Js_of_ocaml.Dom_html.getElementById "canvas1"))
    Graphics_js.open_canvas

   let () = main () *)