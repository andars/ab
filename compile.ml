open Lexer
open Lexing
open Core.Std

let parse_except lexbuf =
  try Parser.program Lexer.token lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%s\n" msg;
    None
  | Parser.Error ->
    fprintf stderr "syntax error\n";
    exit (-1)

let rec compile outc lexbuf =
  match parse_except lexbuf with
  | Some values ->
      let rec help = function
        | [] -> ()
        | (v::vs) ->
          Codegen.codegen outc v;
          help vs;
      in
        help values;
        compile outc lexbuf
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  compile stdout lexbuf;
  In_channel.close inx

let () =
    Command.basic ~summary:"Parse and display arith"
      Command.Spec.(empty +> anon ("filename" %: file))
      loop
    |> Command.run

