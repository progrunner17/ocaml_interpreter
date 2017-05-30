%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN
%token PLUS MINUS TIMES DIV
%token AND OR
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token SEMISEMI

%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI { CExp $1 }
  | LET var EQ expr SEMISEMI { CDecl ($2, $4) }
  | LET var EQ expr LET let_expr { CMultiDecl($2,$4,$6) }
;

let_expr:
  | var EQ expr LET let_expr  { CMultiDecl($1,$3,$5) }
  | var EQ expr SEMISEMI      { CDecl ($1, $3) }
;

expr:
  | LET var EQ expr IN expr     { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | bool_expr                  { $1 }
;


bool_expr:
  | bool_noor_expr OR bool_expr    { EOr($1,$3) }
  | bool_noor_expr                 { $1 }
;

bool_noor_expr:
  | bool_factor_expr AND bool_noor_expr   { EAnd($1,$3) }
  | bool_factor_expr                 { $1 }
;

bool_factor_expr:
  | arith_expr EQ arith_expr    { EEq($1,$3) }
  | arith_expr LT arith_expr    { ELt($1,$3) }
  | arith_expr                  { $1 }
;
arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { EMul($1,$3) }
  | factor_expr DIV atomic_expr   { EDiv($1,$3) }
  | atomic_expr                   { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;

var:
  | ID { $1 }
;


