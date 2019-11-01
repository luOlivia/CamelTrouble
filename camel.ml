<<<<<<< HEAD
open Js_of_ocaml.Dom_html
=======
type pos = {
  mutable x: float;
  mutable y: float;
  mutable angle_degrees: float;
}

type controls =
  | Left
  | Right
  | Up
  | Down

type camel_action =
  | Shooting 
  | CamelMoving
  | Rotating
>>>>>>> 7bb5c0a39e3be3472105ea37b4ab6880ff0b3d1c
