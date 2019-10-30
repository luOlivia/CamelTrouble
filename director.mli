(* Game loop *)
val update : Dom_html.canvasElement Js.t -> float * float -> unit

(* Keydown event handler function *)
val on_key_down : #Dom_html.keyboardEvent Js.t -> bool Js.t

(* Keyup event handler function *)
val on_key_up : #Dom_html.keyboardEvent Js.t -> bool Js.t