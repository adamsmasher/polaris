open Core.Std

type t =
| Var_term of Var.t
| Rec_term of t option ref
| Num_term of int
| String_term of string
| Lam_term of Type_constraint.t list * Type.t list * t
| Closure_term of env * t
| Builtin_term of Type.t * (env -> t)
| App_term of t * Type.t list * t list
| Unit_term
| Seq_term of t * t
| Let_term of t * t
| Letrec_term of Type.t * t * t
| Tuple_term of t list
| Array_term of Type.t * t array
and env = t list
with sexp

let rec to_string = function
| Num_term n -> Int.to_string n
| String_term str -> "\"" ^ str ^ "\""
| Unit_term -> "()"
| Tuple_term ts ->
  "(" ^ String.concat ~sep:", " (List.map ~f:to_string ts) ^ ")"
| Array_term (_, ts) ->
  "[" ^ String.concat_array ~sep:", " (Array.map ~f:to_string ts) ^ "]"
| Var_term v -> Int.to_string v
| Rec_term r ->
  begin match !r with
  | None -> "<rec hole>"
  | Some t -> "<rec " ^ (to_string t) ^ ">"
  end
| Seq_term (t1, t2) -> (to_string t1) ^ "; " ^ (to_string t2)
| Lam_term _ -> "<function>"
| Closure_term _ -> "<function>"
| Builtin_term _ -> "<builtin>"
| App_term (f, tys, args) ->
  (to_string f)
  ^ "<"
  ^ String.concat ~sep:", " (List.map ~f:Type.to_string tys)
  ^ ">"
  ^ "("
  ^ String.concat ~sep:", " (List.map ~f:to_string args)
  ^ ")"
| Let_term (t1, t2) ->
  "let = " ^ (to_string t1) ^ " in " ^ (to_string t2)
| Letrec_term (ty, t1, t2) ->
  "letrec : " ^ (Type.to_string ty) ^ " = "
    ^ (to_string t1) ^ " in " ^ (to_string t2)

module Environment = struct
  let empty = []
  let lookup t v = List.nth_exn t v
  let extend t v = v :: t
  let extend_many t vs = List.fold ~f:extend ~init:t vs
end
