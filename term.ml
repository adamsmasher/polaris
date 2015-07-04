open Core.Std

type t =
| Var_term of Var.t
| Num_term of int
| String_term of string
| Lam_term of (Var.t * Type.t) list * t
| Closure_term of Var.t list * env * t
| Builtin_term of Type.t * Var.t list * (env -> t)
| App_term of t * t list
| Unit_term
| Seq_term of t * t
| Let_term of Var.t * t * t
| Tuple_term of t list
| Array_term of Type.t * t array
and env = (Var.t * t) list
with sexp

module Environment = struct
  let empty = []
  let lookup t v = List.Assoc.find_exn t v
  let extend t v = v :: t
  let extend_many t vs = List.fold ~f:extend ~init:t vs
end
