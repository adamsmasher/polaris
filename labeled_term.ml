open Core.Std

type t =
| Var_term of string
| Num_term of int
| String_term of string
| Lam_term of Type_constraint.t list * (string * Type.t) list * t
| App_term of t * Type.t list * t list
| Unit_term
| Seq_term of t * t
| Let_term of string * t * t
| Letrec_term of string * Type.t * t * t
| Tuple_term of t list
| Array_term of Type.t * t array
with of_sexp

let rec to_term env = function
| Var_term s ->
  let index, _ =
    Option.value_exn (List.findi env ~f:(fun _ s' -> s = s'))
  in
  Term.Var_term index
| Num_term n -> Term.Num_term n
| String_term s -> Term.String_term s
| Lam_term (ty_constraints, labeled_params, body) ->
  let params = List.map ~f:Tuple.T2.get2 labeled_params in
  let extended_env =
    List.fold labeled_params ~init:env ~f:(fun env (s,_) -> s::env)
  in
  Term.Lam_term (ty_constraints, params, to_term extended_env body)
| App_term (t, tys, args) ->
  Term.App_term (to_term env t, tys, List.map ~f:(to_term env) args)
| Unit_term -> Term.Unit_term
| Seq_term (t1, t2) -> Term.Seq_term (to_term env t1, to_term env t2)
| Let_term (s, t1, t2) -> Term.Let_term (to_term env t1, to_term (s::env) t2)
| Letrec_term (s, ty, t1, t2) ->
  Term.Letrec_term (ty, to_term env t1, to_term (s::env) t2)
| Tuple_term ts -> Term.Tuple_term (List.map ~f:(to_term env) ts)
| Array_term (ty, ts) -> Term.Array_term (ty, Array.map ~f:(to_term env) ts)
