open Js_of_ocaml

type t = 
  {maze: Maze.t;
   camels: Camel.t list option;
   balls: Ball.t list option;
   ctx: Dom_html.canvasRenderingContext2D Js.t;
  }

let update_loop canvas =
  (* let ctx = canvas##getContext (Dom_html._2d_) in
     let state = {maze=maze; camels=Some camels; balls=None; ctx=ctx} in 
     let rec update_helper  *)
  failwith "unimplemented"