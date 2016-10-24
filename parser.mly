%token <int> INT
%token <string> IDENT
%token AUTO EXTRN
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token COLON
%token RETURN
%token PLUS MINUS
%token STAR DIV
%token EQUALS
%token EOF

(* lowest *)
%left EQUALS
%left PLUS MINUS
%left STAR DIV
(* highest *)

%start <Ast.ast list option> program
%%

program:
  | ds = definition*; EOF     { Some ds }
  | EOF                       { None }
  ;

definition:
  | id = IDENT; SEMICOLON                       { `Global id }
  | f = IDENT; LPAREN; RPAREN; s = statement    {`Function (f, s)}
  ;

(*
ival:
  | c = int { `int c }
  | name = ident { `var name }
  ;
*)

statement:
  | AUTO; id = IDENT; SEMICOLON;            {`Local id }
  | EXTRN; id = IDENT; SEMICOLON;           {`External id }
  | l = IDENT; COLON;                       {`Label l }
  | RETURN; v = rvalue? SEMICOLON           {`Return v }
  | LBRACE; stmts = statement*; RBRACE      {`Compound stmts }
  | v = rvalue? SEMICOLON
    { match v with
        | Some node -> node
        | None -> `Nothing
    }
  ;

rvalue:
  | LPAREN; v = rvalue; RPAREN          { v }
  | l = lvalue                          { l }
  | i = INT                             { `Int i }
  | l = lvalue; t = assign; r = rvalue  { `Assignment (t, l, r) }
  | l = rvalue; o = binary; r = rvalue  { `Binary (o, l, r) }
  ;

%inline binary:
  | PLUS  { "+" }
  | MINUS { "-" }
  | STAR  { "*" }
  | DIV   { "/" }
  ;

%inline assign:
  | EQUALS { "=" }
  ;

lvalue:
  | id = IDENT; { `Var id }
  ;
