open Lexer
open Lexing
open Core.Std

let rec parse_and_print lexbuf =
  match Parser.prog Lexer.token lexbuf with
  | Some value ->
      printf "%a\n" Ast.dump value;
      parse_and_print lexbuf
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let () =
    Command.basic ~summary:"Parse and display arith"
      Command.Spec.(empty +> anon ("filename" %: file))
      loop
    |> Command.run

