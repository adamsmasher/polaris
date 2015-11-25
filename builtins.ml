open Core.Std

let get_builtin_ty = function
| Term.Builtin_term (ty, _) -> ty
| _ -> assert false

let make_bin_op op =
  let ty = Type.(Fun_type ([], [Num_type; Num_type], Num_type)) in
  Term.(Builtin_term (ty, fun env ->
    let (t1, t2) = Environment.(lookup env 0, lookup env 1) in
    match t1, t2 with
    | Num_term n1, Num_term n2 -> Num_term (op n1 n2)
    | _ -> assert false))

let add_int = make_bin_op (+)
let sub_int = make_bin_op (-)
let mul_int = make_bin_op ( * ) 
let div_int = make_bin_op (/)

let int_to_string =
  let ty = Type.(Fun_type ([], [Num_type], String_type)) in
  Term.(Builtin_term (ty, fun env ->
    let t = Environment.lookup env 0 in
    match t with
    | Num_term n -> String_term (Int.to_string n)
    | _ -> assert false))

let print_string =
  let ty = Type.(Fun_type ([], [String_type], Unit_type)) in
  Term.(Builtin_term (ty, fun env ->
    let t = Environment.lookup env 0 in
    match t with
    | String_term str -> print_string str; Unit_term
    | _ -> assert false))

let string_length =
  let ty = Type.(Fun_type ([], [String_type], Num_type)) in
  Term.(Builtin_term (ty, fun env ->
    let t = Environment.lookup env 0 in
    match t with
    | String_term str -> Num_term (String.length str)
    | _ -> assert false))

let array_length =
  let ty =
    Type.(Fun_type ([()], [Array_type (Var_type 0)], Num_type))
  in
  Term.(Builtin_term (ty, fun env ->
    let t = Environment.lookup env 0 in
    match t with
    | Array_term (_, ts) -> Num_term (Array.length ts)
    | _ -> assert false))

let default_type_environment =
  let open Type_environment in
  extend_many empty [
    get_builtin_ty add_int;
    get_builtin_ty sub_int;
    get_builtin_ty mul_int;
    get_builtin_ty div_int;
    get_builtin_ty int_to_string;
    get_builtin_ty print_string;
    get_builtin_ty array_length;
    get_builtin_ty string_length;
  ] 

let default_environment =
  let open Term.Environment in
  extend_many empty [
    add_int;
    sub_int;
    mul_int;
    div_int;
    int_to_string;
    print_string;
    array_length;
    string_length;
  ]
