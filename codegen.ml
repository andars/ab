let enclosing = Stack.create();;

let emit outc (str:string) =
  output_string outc (str ^ "\n");
;;

let emitf outc =
  Printf.fprintf outc
;;

let emit_label outc (label:string) =
  emit outc (label ^ ":");
;;

let section outc (s:string) =
  emit outc ("." ^ s);
;;

let binary_insn_for = function
  | "+" -> "addq"
  | "-" -> "subq"
  | "*" -> "imul"
  | _ -> "UNKNOWN"
;;

let rec emit_expr outc expr =
  let emit_binary (op, lhs, rhs) =
    emit_expr outc lhs;
    emit outc "movq %rax, %rbx";
    emit_expr outc rhs;
    emitf outc "%s %%rbx, %%rax\n" (binary_insn_for op)
  in
  match expr with
  | `Int i ->
      emitf outc "movq $%d, %%rax\n" i
  | `Binary b -> emit_binary b
  | _ -> ()
;;

let emit_return outc = function
  | Some expr ->
      emit_expr outc expr;
      emit outc ("jmp " ^ (Stack.top enclosing) ^ "_end")
  | None -> ()
;;

let rec emit_stmt outc stmt = match stmt with
  | `Compound stmts ->
      List.iter (emit_stmt outc) stmts
  | `Return v ->
      emit_return outc v
  | `Binary _ | `Int _ ->
      emit_expr outc stmt
  | `Label l ->
      emit_label outc l (*TODO mangle to prevent collisions*)
  | `Nothing -> ()
  | _ -> ()
;;

let emit_function outc (name, body) =
  let emit_function_prologue () =
    emit outc "pushq %rbp";
    emit outc "movq %rsp, %rbp";
  in
  let emit_function_epilogue () =
    emit outc "movq %rbp, %rsp";
    emit outc "popq %rbp";
    emit outc "retq";
  in
    Stack.push name enclosing;
    emit outc (".globl " ^ name);
    emit_label outc name;
    emit_function_prologue ();
    emit_stmt outc body;
    emit_label outc (name ^ "_end");
    emit_function_epilogue ();
    ignore (Stack.pop enclosing);
;;

let emit_global_decl outc name =
  section outc "data";
  emit_label outc name;
  emit outc ".quad 42"; (* TODO: add value to globals *)
  section outc "text";
;;


let codegen outc = function
  | `Function f -> emit_function outc f
  | `Global name -> emit_global_decl outc name
  | _ -> ()
;;

