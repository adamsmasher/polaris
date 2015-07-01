type t
val empty : t
val lookup : t -> Var.t -> Type.t
val extend : t -> Type.t -> t
