open Core.Std

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Interpreter for the Polaris programming language"
    spec
    (fun filename () ->
      Sexp.load_sexps_conv_exn filename Term.t_of_sexp
      |> List.iter ~f:(fun term ->
        let ty = Typecheck.type_of Type_environment.empty term in
        let result = Eval.eval Term.Environment.empty term in
        Printf.printf "%s : %s\n"
          (Sexp.to_string (Term.sexp_of_t result))
          (Sexp.to_string (Type.sexp_of_t ty))))

let () =
  Command.run command
