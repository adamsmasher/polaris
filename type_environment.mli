type t
val empty : t
val lookup : t -> Var.t -> Type.t
val extend : t -> Var.t * Type.t -> t
val extend_many : t -> (Var.t * Type.t) list -> t
