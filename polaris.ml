open Core.Std

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let run_file filename =
  let sexps = Sexp.load_sexps_conv_exn filename Term.t_of_sexp in
  let env, ty_env = Builtins.(default_environment, default_type_environment) in
  List.iter sexps ~f:(fun term ->
    let ty = Typecheck.type_of ty_env term in
    let result = Eval.eval env term in
    Printf.printf "%s : %s\n" (Term.to_string result) (Type.to_string ty))

let command =
  Command.basic
    ~summary:"Interpreter for the Polaris programming language"
    spec
    (fun filename () -> run_file filename)

let () =
  Command.run command
