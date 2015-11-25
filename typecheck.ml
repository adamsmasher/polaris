open Core.Std

exception Type_error of Type.t * Type.t
exception Invalid_application of Type.t
exception Forall_length_mismatch
exception Ty_constraint_error of Type_constraint.t list * Type_constraint.t list

let match_constraints ty_constraints1 ty_constraints2 =
  let rec f ty_constraints1 ty_constraints2 =
    match ty_constraints1, ty_constraints2 with
    | [], [] -> ()
    | ty_constraint1::ty_constraints1, ty_constraint2::ty_constraints2 ->
      Type_constraint.match_constraints ty_constraint1 ty_constraint2;
      f ty_constraints1 ty_constraints2
    | _, _ -> raise Forall_length_mismatch
  in
  try f ty_constraints1 ty_constraints2
  with Forall_length_mismatch ->
    raise (Ty_constraint_error (ty_constraints1, ty_constraints2))

let rec match_type ty1 ty2 =
  let open Type in
  let match_types tys1 tys2 =
    try List.iter2_exn ~f:match_type tys1 tys2
    with Invalid_argument _ -> raise (Type_error (ty1, ty2))
  in
  let error () = raise (Type_error (ty1, ty2)) in
  match ty1, ty2 with
  | Num_type, Num_type -> ()
  | Num_type, _ -> error ()
  | String_type, String_type -> ()
  | String_type, _ -> error ()
  | Fun_type (ty_constraints1, tys1, return_ty1),
    Fun_type (ty_constraints2, tys2, return_ty2) ->
    match_constraints ty_constraints1 ty_constraints2;
    match_types tys1 tys2;
    match_type return_ty1 return_ty2
  | Fun_type _, _ -> error ()
  | Unit_type, Unit_type -> ()
  | Unit_type, _ -> error ()
  | Tuple_type tys1, Tuple_type tys2 -> match_types tys1 tys2
  | Tuple_type _, _ -> error ()
  | Array_type ty1, Array_type ty2 -> match_type ty1 ty2
  | Array_type _, _ -> error ()
  | Var_type _, _ -> ()

let return_type t =
  let open Type in
  match t with
  | Fun_type (_, _, return_ty) -> return_ty
  | ty -> raise (Invalid_application ty)

let rec substitute ty n = let open Type in function
| Num_type -> Num_type
| String_type -> String_type
| Fun_type (ty_constraints, param_tys, return_ty) ->
  let n' = n + (List.length ty_constraints) in
  Fun_type
    (ty_constraints,
     List.map ~f:(substitute ty n') param_tys,
     substitute ty n' return_ty)
| Unit_type -> Unit_type
| Tuple_type tys -> Tuple_type (List.map ~f:(substitute ty n) tys)
| Array_type ty' -> Array_type (substitute ty n ty')
| Var_type n' when n = n' -> ty
| Var_type _ as ty -> ty

let apply_tys tys = let open Type in function
| Fun_type (ty_constraints, param_tys, return_type) ->
  if (List.length tys) <> (List.length ty_constraints) then
    raise Forall_length_mismatch;
  let param_tys =
    List.map param_tys ~f:(fun param_ty ->
      List.fold tys ~init:param_ty ~f:(fun param_ty ty -> substitute ty 0 param_ty))
  in
  let return_type =
    List.fold tys ~init:return_type ~f:(fun return_type ty ->
      substitute ty 0 return_type)
  in
  Fun_type ([], param_tys, return_type)
| ty -> raise (Invalid_application ty)

let rec type_of ty_env t =
  let open Term in
  let open Type in
  match t with
  | Var_term x -> Type_environment.lookup ty_env x
  (* rec holes are only created during evaluation *)
  | Rec_term _ -> assert false
  | Num_term _ -> Num_type
  | String_term _ -> String_type
  | Lam_term (ty_constraints, param_tys, body) ->
    let extended_type_env = Type_environment.extend_many ty_env param_tys in
    Fun_type (ty_constraints, param_tys, (type_of extended_type_env body))
  | Closure_term _ ->
    (* closures can only be created during evaluation *)
    assert false 
  | Builtin_term (ty, _) -> ty
  | App_term (f, tys, ts) ->
    let f_type = apply_tys tys (type_of ty_env f) in
    let arg_tys = List.map ~f:(type_of ty_env) ts in
    let return_type = return_type f_type in
    match_type f_type (Fun_type ([], arg_tys, return_type));
    return_type
  | Unit_term -> Unit_type
  | Seq_term (t1, t2) ->
    match_type (type_of ty_env t1) Unit_type;
    type_of ty_env t2
  | Let_term (t1, t2) ->
    let extended_env = Type_environment.extend ty_env (type_of ty_env t1) in
    type_of extended_env t2
  | Letrec_term (ty, t1, t2) ->
    let extended_env = Type_environment.extend ty_env ty in
    let ty1 = type_of extended_env t1 in
    match_type ty ty1;
    let ty2 = type_of extended_env t2 in
    ty2
  | Tuple_term ts -> Tuple_type (List.map ~f:(type_of ty_env) ts)
  | Array_term (ty, ts) ->
    Array.iter ts ~f:(fun t -> match_type ty (type_of ty_env t));
    Array_type ty
