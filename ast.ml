type ast = [
  | `Binary of (string * ast * ast)
  | `Int of int
  | `Assignment of (string * ast * ast)
  | `Var of string
  | `Function of (string * ast)
  | `Global of string
  | `Local of string
  | `External of string
  | `Compound of ast list
  | `Return of ast option
  | `Label of string
  | `Nothing
]

open Core.Std
let rec dump_ast ch = function
  | `Binary (op, l, r) -> printf "(%s %a %a)" op dump_ast l dump_ast r
  | `Int i      -> printf "%d" i
  | `Var s      -> printf "(Var %s)" s
  | `Assignment (t, l, r) -> printf "(%s %a %a)" t dump_ast l dump_ast r
  | `Global name -> printf "Global: %s\n" name
  | `Local name -> printf "Local: %s\n" name
  | `External name -> printf "External: %s\n" name
  | `Compound stmts -> dump_compound ch stmts
  | `Function f -> dump_function ch f;
  | `Return v -> (match v with
    | Some node ->
        printf "Return: %a\n" dump_ast node;
    | None -> printf "Return\n";)
  | `Label l -> printf "label %s:\n" l
  | `Nothing -> printf "\n"

and dump_compound ch stmts =
  output_string ch "{\n";
  List.iter ~f:(fun stmt ->
    printf "%a" dump_ast stmt;
  ) stmts;
  output_string ch "}\n"

and dump_function ch (name, stmt) =
  printf "Function: %s\n" name;
  dump_ast ch stmt;

