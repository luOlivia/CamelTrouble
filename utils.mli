(** Common utility methods *)

(** [cosine degree] is cosine of degrees *)
val cosine : float -> float

(** [sine degree] is sine of degrees *)
val sine : float -> float

(** [truncate x] is [x] with its decimal places zeroed out *)
val truncate : float -> float

(** [difference l1 l2] is the set-like difference of lists [l1] and [l2] *)
val difference : 'a list -> 'a list -> 'a list