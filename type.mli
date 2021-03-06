type t =
| Num_type
| String_type
| Fun_type of Type_constraint.t list * t list * t
| Unit_type
| Tuple_type of t list
| Array_type of t
| Var_type of int
with sexp

val to_string : t -> string
