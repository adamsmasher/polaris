open Core.Std

type t =
| Num_type
| String_type
| Fun_type of Type_constraint.t list * t list * t
| Unit_type
| Tuple_type of t list
| Array_type of t
| Var_type of int
with sexp

let rec to_string = function
| Num_type -> "Num"
| String_type -> "String"
| Fun_type (ty_constraints, tys, ty) ->
  let forall =
    if List.is_empty ty_constraints then ""
    else
      "forall "
      ^ String.concat ~sep:" " (List.init (List.length ty_constraints) ~f:Int.to_string)
      ^ ": "
  in
  forall ^ "(" ^ String.concat ~sep:", " (List.map ~f:to_string tys) ^ ") -> "
  ^ (to_string ty)
| Unit_type -> "()"
| Tuple_type tys ->
  "(" ^ String.concat ~sep:", " (List.map ~f:to_string tys) ^ ")"
| Array_type ty -> "[" ^ to_string ty ^ "]"
| Var_type n -> Int.to_string n
