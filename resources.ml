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
  "           _.\'   _.-\'       )     \"-._    ||\\\\   |\\\\";
  "         \'               .-\'          `\'  || \\\\  ||))";
  "_____  _  ___  _ ____________ _____  ___ _|\\ _|\\_|\\\\/";
  "                       c  c  \" c C \"\"C  \" \"\"  \"\" \"\""
]



let cameltext = [
  " $$$$$$\\   $$$$$$\\  $$\\      $$\\ $$$$$$$$\\ $$\\       ";
  "$$  __$$\\ $$  __$$\\ $$$\\    $$$ |$$  _____|$$ |      ";
  "$$ /  \\__|$$ /  $$ |$$$$\\  $$$$ |$$ |      $$ |      ";
  "$$ |      $$$$$$$$ |$$\\$$\\$$ $$ |$$$$$\\    $$ |      ";
  "$$ |      $$  __$$ |$$ \\$$$  $$ |$$  __|   $$ |      ";
  "$$ |  $$\\ $$ |  $$ |$$ |\\$  /$$ |$$ |      $$ |      ";
  "\$$$$$$  |$$ |  $$ |$$ | \\_/ $$ |$$$$$$$$\\ $$$$$$$$\\ ";
  " \\______/ \\__|  \\__|\\__|     \\__|\\________|\\________|"
]
let troubletext = [
  "$$$$$$$$\\ $$$$$$$\\   $$$$$$\\  $$\\   $$\\ $$$$$$$\\  $$\\       $$$$$$$$\\ ";
  "\\__$$  __|$$  __$$\\ $$  __$$\\ $$ |  $$ |$$  __$$\\ $$ |      $$  _____|";
  "   $$ |   $$ |  $$ |$$ /  $$ |$$ |  $$ |$$ |  $$ |$$ |      $$ |      ";
  "   $$ |   $$$$$$$  |$$ |  $$ |$$ |  $$ |$$$$$$$\\ |$$ |      $$$$$\\    ";
  "   $$ |   $$  __$$< $$ |  $$ |$$ |  $$ |$$  __$$\\ $$ |      $$  __|   ";
  "   $$ |   $$ |  $$ |$$ |  $$ |$$ |  $$ |$$ |  $$ |$$ |      $$ |      ";
  "   $$ |   $$ |  $$ | $$$$$$  |\\$$$$$$  |$$$$$$$  |$$$| $$$$$$$$\\ ";
  "   \\__|   \\__|  \\__| \\______/  \\______/ \\_______/ \\__|\\________|"
]

let rec draw_ascii word x y = 
  match word with
  | [] -> ()
  | h::t -> Graphics.moveto x y; Graphics.draw_string h; draw_ascii t x (y-10)

let draw word x y = 
  if word = "camel" then draw_ascii cameltext x y
  else if word = "trouble" then draw_ascii troubletext x y
  else draw_ascii desert x y

let n = 0x000000

and w = 0xFFFFFF

and b = 0xFFCC99

and y = 0xFFFF00

and o = 0xCC9966

and v = 0x00BB00

and g = 0x888888

and c = 0xDDDDDD

and t = Graphics.transp 

let caml =
  [| [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t |]
   ; [| n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; n
      ; n
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t |]
   ; [| n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t |]
   ; [| n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t |]
   ; [| n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; g
      ; g
      ; g
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t |]
   ; [| n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; c
      ; c
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t |]
   ; [| t
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; c
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; b
      ; b
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; b
      ; o
      ; b
      ; b
      ; n
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; o
      ; n
      ; n
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; b
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; o
      ; n
      ; b
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; b
      ; b
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; n
      ; b
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; y
      ; v
      ; y
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; v
      ; y
      ; o
      ; o
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; o
      ; o
      ; o
      ; y
      ; v
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; o
      ; v
      ; y
      ; o
      ; y
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; b
      ; b
      ; n
      ; n
      ; b
      ; b
      ; n |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; o
      ; y
      ; y
      ; o
      ; o
      ; v
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; b
      ; b
      ; b
      ; n
      ; n
      ; n
      ; b
      ; n
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; v
      ; o
      ; v
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; b
      ; n
      ; n
      ; n
      ; n
      ; b
      ; n
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; t
      ; n
      ; v
      ; o
      ; o
      ; v
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; o
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t |]
   ; [| t
      ; t
      ; t
      ; t
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; n
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t
      ; t |] |] 

