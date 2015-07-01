exception Type_error of Type.t * Type.t
exception Invalid_application of Type.t

val type_of : Type_environment.t -> Term.t -> Type.t
