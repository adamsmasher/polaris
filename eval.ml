open Core.Std

let rec eval env t =
  let open Term in
  match t with
  | Var_term x -> Environment.lookup env x
  | Num_term _ as n -> n
  | Lam_term (args, body) ->
    let arg_names = List.map ~f:fst args in
    Closure_term (arg_names, env, body)
  | Closure_term _ as f -> f
  | Unit_term -> Unit_term
  | Seq_term (t1, t2) ->
    let _ = eval env t1 in
    eval env t2
  | Let_term (var, t1, t2) ->
    let v = eval env t1 in
    eval (Environment.extend env (var, v)) t2
  | Tuple_term ts -> Tuple_term (List.map ~f:(eval env) ts)
  | Array_term (ty, ts) -> Array_term (ty, Array.map ~f:(eval env) ts)
  | App_term (t, ts) ->
    match eval env t with
    | Closure_term (vars, env, body) ->
      let args = List.map ~f:(eval env) ts in
      eval
        (Environment.extend_many env (List.zip_exn vars args))
        body
    | _ -> assert false
