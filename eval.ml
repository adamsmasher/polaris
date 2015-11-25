open Core.Std

let rec eval env t =
  let open Term in
  match t with
  | Var_term x -> eval env (Environment.lookup env x)
  | Rec_term r ->
    begin match !r with
    | Some t -> t
    | None -> assert false
    end
  | Num_term _ as n -> n
  | String_term _ as s -> s
  | Lam_term (_, _, body) -> Closure_term (env, body)
  | Closure_term _ as f -> f
  | Builtin_term _ as f -> f
  | Unit_term -> Unit_term
  | Seq_term (t1, t2) ->
    let _ = eval env t1 in
    eval env t2
  | Let_term (t1, t2) ->
    let v = eval env t1 in
    eval (Environment.extend env v) t2
  | Letrec_term (_, t1, t2) ->
    let v_ref = ref None in
    let v = eval (Environment.extend env (Rec_term v_ref)) t1 in
    v_ref := Some v;
    eval (Environment.extend env v) t2
  | Tuple_term ts -> Tuple_term (List.map ~f:(eval env) ts)
  | Array_term (ty, ts) -> Array_term (ty, Array.map ~f:(eval env) ts)
  | App_term (t, _, ts) ->
    let args = List.map ~f:(eval env) ts in
    match eval env t with
    | Closure_term (env, body) -> eval (Environment.extend_many env args) body
    | Builtin_term (_, f) -> f Environment.(extend_many empty args)
    | t -> Printf.printf "Bad apply %s\n" (Term.to_string t); assert false
