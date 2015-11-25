open Core.Std

type t = Type.t list
let empty = []
let lookup t v = List.nth_exn t v
let extend t ty = ty :: t
let extend_many t tys = List.fold ~f:extend ~init:t tys
