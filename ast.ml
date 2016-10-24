type ast = [
  | `BinOp of (string * ast * ast)
  | `Int of int
  | `Assignment of (string * ast * ast)
  | `Var of string
]

open Core.Std
let rec dump ch = function
  | `BinOp (op, l, r)   -> printf "(%s %a %a)" op dump l dump r
  | `Int i      -> printf "%d" i
  | `Var s      -> printf "%s" s
  | `Assignment (t, l, r) -> printf "(%s %a %a)" t dump l dump r
