type env

type t =
| Var_term of Var.t
| Rec_term of t option ref
| Num_term of int
| String_term of string
| Lam_term of Type.t list * t
| Forall_term of Type_constraint.t list * t
| Closure_term of env * t
| Builtin_term of Type.t * (env -> t)
| App_term of t * t list
| Unit_term
| Seq_term of t * t
| Let_term of t * t
| Letrec_term of Type.t * t * t
| Tuple_term of t list
| Array_term of Type.t * t array
with sexp

val to_string : t -> string

module Environment : sig
  val empty : env
  val lookup : env -> Var.t -> t
  val extend : env -> t -> env
  val extend_many : env -> t list -> env
end
