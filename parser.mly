%token <int> INT
%token <string> IDENT
%token LPAREN
%token RPAREN
%token PLUS MINUS
%token STAR DIV
%token EQUALS
%token EOF
%left EQUALS
%left PLUS MINUS
%left STAR DIV

%start <Ast.ast option> prog
%%

prog:
  | EOF { None }
  | v = rvalue; EOF { Some v }
  ;

rvalue:
  | LPAREN; v = rvalue; RPAREN      { v }
  | l = lvalue                      { l }
  | i = INT                         { `Int i }
  | l = lvalue; t = assign; r = rvalue  { `Assignment (t, l, r) }
  | l = rvalue; o = binary; r = rvalue    { `BinOp (o, l, r) }
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
