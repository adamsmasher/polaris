open Core.Std

type t =
| Num_type
| String_type
| Fun_type of t list * t
| Unit_type
| Tuple_type of t list
| Array_type of t
| Forall_type of t
| Var_type of int
with sexp

let rec to_string = function
| Num_type -> "Num"
| String_type -> "String"
| Fun_type (tys, ty) ->
  "(" ^ String.concat ~sep:", " (List.map ~f:to_string tys) ^ ") -> "
  ^ (to_string ty)
| Unit_type -> "()"
| Tuple_type tys ->
  "(" ^ String.concat ~sep:", " (List.map ~f:to_string tys) ^ ")"
| Array_type ty -> "[" ^ to_string ty ^ "]"
| Forall_type ty -> "forall. " ^ (to_string ty)
| Var_type n -> Int.to_string n
