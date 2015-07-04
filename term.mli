type env

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
with sexp

val to_string : t -> string

module Environment : sig
  val empty : env
  val lookup : env -> Var.t -> t
  val extend : env -> Var.t * t -> env
  val extend_many : env -> (Var.t * t) list -> env
end
