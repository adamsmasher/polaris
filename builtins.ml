open Core.Std

let get_builtin_ty = function
| Term.Builtin_term (ty, _, _) -> ty
| _ -> assert false

let make_bin_op op =
  let ty = Type.(Fun_type ([Num_type; Num_type], Num_type)) in
  Term.(Builtin_term (ty, ["x"; "y"], fun env ->
    let (t1, t2) = Environment.(lookup env "x", lookup env "y") in
    match t1, t2 with
    | Num_term n1, Num_term n2 -> Num_term (op n1 n2)
    | _ -> assert false))

let add_int = make_bin_op (+)
let sub_int = make_bin_op (-)
let mul_int = make_bin_op ( * ) 
let div_int = make_bin_op (/)

let int_to_string =
  let ty = Type.(Fun_type ([Num_type], String_type)) in
  Term.(Builtin_term (ty, ["x"], fun env ->
    let t = Environment.lookup env "x" in
    match t with
    | Num_term n -> String_term (Int.to_string n)
    | _ -> assert false))

let default_type_environment =
  let open Type_environment in
  extend_many empty [
    "_add_int", get_builtin_ty add_int;
    "_sub_int", get_builtin_ty sub_int;
    "_mul_int", get_builtin_ty mul_int;
    "_div_int", get_builtin_ty div_int;
    "_int_to_string", get_builtin_ty int_to_string;
  ] 

let default_environment =
  let open Term.Environment in
  extend_many empty [
    "_add_int", add_int;
    "_sub_int", sub_int;
    "_mul_int", mul_int;
    "_div_int", div_int;
    "_int_to_string", int_to_string;
  ]
