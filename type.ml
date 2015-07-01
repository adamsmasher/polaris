open Core.Std

type t =
| Num_type
| Fun_type of t list * t
| Unit_type
| Tuple_type of t list
| Array_type of t
with sexp
