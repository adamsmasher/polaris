open Core.Std

type t = Type.t list
let empty = []
let lookup = List.nth_exn
let extend t ty = ty :: t
