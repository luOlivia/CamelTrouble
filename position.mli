(**[t] is the position in a 2D cartesian coordinate system represented by 
   (x,y) *)
type t = {x: float; y: float}

(**[distance p1 p2] is the euclidean distance between [p1] and [p2]*)
val distance : t -> t -> float

(**[init nx ny] is a position with x coordinate [nx] and y coordinate [ny]*)
val init : float -> float -> t 