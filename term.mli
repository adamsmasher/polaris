type env

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
with sexp

module Environment : sig
  val empty : env
  val lookup : env -> Var.t -> t
  val extend : env -> t -> env
  val extend_many : env -> t list -> env
end
