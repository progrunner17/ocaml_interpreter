%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN LETAND
%token PLUS MINUS TIMES DIV MOD
%token AND OR
%token EQ LT LE GT GE
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW DFUN
%token REC
%token SEMISEMI
%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI                       { CExp $1 }
  | LET var let_expr SEMISEMI           { CDecl ($2, $3) }
  | LET REC var var let_expr SEMISEMI    { CRecDecl ($3,$4,$5) }
;

let_expr:
  | EQ expr                             { $2 }
  | var let_expr                        { EFun($1,$2) }
;


expr:
  | LET var let_expr IN expr            { ELet($2,$3,$5) }
  | LET REC var var EQ expr IN expr     { ELetRec($3,$4,$6,$8) }
  | IF expr THEN expr ELSE expr         { EIf($2,$4,$6) }
  | FUN fun_abbr                        { $2 }
  | bool_expr                           { $1 }
;

fun_abbr:
  |var ARROW expr                        { EFun($1,$3) }
  |var fun_abbr                          { EFun($1,$2) }
;


bool_expr:
  | bool_or_expr OR bool_expr           { EOr($1,$3) }
  | bool_or_expr                        { $1 }
;

bool_or_expr:
  | bool_factor_expr AND bool_or_expr   { EAnd($1,$3) }
  | bool_factor_expr                    { $1 }
;

bool_factor_expr:
  | bool_factor_expr EQ arith_expr       { EEq($1,$3) }
  | bool_factor_expr LT arith_expr       { ELt($1,$3) }
  | bool_factor_expr LE arith_expr       { ELe($1,$3) }
  | bool_factor_expr GT arith_expr       { EGt($1,$3) }
  | bool_factor_expr GE arith_expr       { EGe($1,$3) }
  | arith_expr                           { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr          { EAdd($1,$3) }
  | arith_expr MINUS factor_expr         { ESub($1,$3) }
  | factor_expr                          { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr        { EMul($1,$3) }
  | factor_expr DIV atomic_expr          { EDiv($1,$3) }
  | factor_expr MOD atomic_expr          { EMod($1,$3) }
  | app_expr                          { $1 }
;

app_expr:
  | app_expr atomic_expr { EApp($1, $2) }
  | atomic_expr          { $1 }

;

atomic_expr:
  | INT                                  { EConstInt($1) }
  | BOOL                                 { EConstBool($1) }
  | ID                                   { EVar($1) }
  | LPAR expr RPAR                       { $2 }
;

var:
  | ID { $1 }
;


