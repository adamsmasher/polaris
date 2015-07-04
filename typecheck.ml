open Core.Std

exception Type_error of Type.t * Type.t
exception Invalid_application of Type.t

let rec match_type ty1 ty2 =
  let open Type in
  let match_types tys1 tys2 =
    try List.iter2_exn ~f:match_type tys1 tys2
    with Invalid_argument _ -> raise (Type_error (ty1, ty2))
  in
  match ty1, ty2 with
  | Num_type, Num_type -> ()
  | String_type, String_type -> ()
  | Fun_type (tys1, return_ty1), Fun_type (tys2, return_ty2) ->
    match_types tys1 tys2;
    match_type return_ty1 return_ty2
  | Unit_type, Unit_type -> ()
  | Tuple_type tys1, Tuple_type tys2 -> match_types tys1 tys2
  | Array_type ty1, Array_type ty2 -> match_type ty1 ty2
  | _, _ -> raise (Type_error (ty1, ty2))

let return_type t =
  let open Type in
  match t with
  | Fun_type (_, return_ty) -> return_ty
  | ty -> raise (Invalid_application ty)

let rec type_of ty_env t =
  let open Term in
  let open Type in
  match t with
  | Var_term x -> Type_environment.lookup ty_env x
  | Num_term _ -> Num_type
  | String_term _ -> String_type
  | Lam_term (args, body) ->
    let extended_type_env = Type_environment.extend_many ty_env args in
    let tys = List.map ~f:snd args in
    Fun_type (tys, (type_of extended_type_env body))
  | Closure_term _ ->
    (* closures can only be created during evaluation *)
    assert false 
  | Builtin_term (ty, _, _) -> ty
  | App_term (f, ts) ->
    let f_type = type_of ty_env f in
    let tys = List.map ~f:(type_of ty_env) ts in
    let return_type = return_type f_type in
    match_type f_type (Fun_type (tys, return_type));
    return_type
  | Unit_term -> Unit_type
  | Seq_term (t1, t2) ->
    match_type (type_of ty_env t1) Unit_type;
    type_of ty_env t2
  | Let_term (var, t1, t2) ->
    let extended_env =
      Type_environment.extend ty_env (var, (type_of ty_env t1))
    in
    type_of extended_env t2
  | Tuple_term ts -> Tuple_type (List.map ~f:(type_of ty_env) ts)
  | Array_term (ty, ts) ->
    Array.iter ts ~f:(fun t -> match_type ty (type_of ty_env t));
    Array_type ty
