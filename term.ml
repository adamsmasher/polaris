open Core.Std

type t =
| Var_term of Var.t
| Num_term of int
| Lam_term of Type.t list * t
| Closure_term of Type.t list * env * t
| App_term of t * t list
| Unit_term
| Seq_term of t * t
| Let_term of t * t
| Tuple_term of t list
| Array_term of Type.t * t array
and env = t list
with sexp

module Environment = struct
  let empty = []
  let lookup = List.nth_exn
  let extend t v = v :: t
  let extend_many t vs = List.fold ~f:extend ~init:t vs
end
