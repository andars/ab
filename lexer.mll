{
  open Parser
  open Lexing

  exception SyntaxError of string
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | [' ' '\n' '\t']      { token lexbuf }
  | int             { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "auto"          { AUTO }
  | "extrn"         { EXTRN }
  | "return"        { RETURN }
  | ident           { IDENT (Lexing.lexeme lexbuf) }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { DIV }
  | '='             { EQUALS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ';'             { SEMICOLON }
  | ':'             { COLON }
  | _
      { raise (SyntaxError ("Unexpected: " ^ Lexing.lexeme lexbuf))}
  | eof             { EOF }
