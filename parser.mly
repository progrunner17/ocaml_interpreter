%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN LETAND REC
%token PLUS MINUS TIMES DIV
%token AND OR
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW
%token SEMISEMI

%start toplevel
%type <Syntax.command> toplevel
%%

toplevel:
  | expr SEMISEMI                       { CExp $1 }
  | LET let_expr SEMISEMI               { $2 }
  | LET REC var var EQ expr SEMISEMI         { CRecDecl($3,$4,$6) }

;

let_expr:
  | var EQ expr LET let_expr            { CMultiDecl($1,$3,$5) }
  | var EQ expr LETAND let_expr         { CAndDecl($1,$3,$5) }
  | var EQ expr                         { CDecl ($1, $3) }
  | var EQ expr                         { CDecl ($1, $3) }
;

;
expr:
  | LET var EQ expr IN expr             { ELet($2,$4,$6) }
  | LET REC var var EQ expr IN expr     { ELetRec($3,$4,$6,$8) }
  | IF expr THEN expr ELSE expr         { EIf($2,$4,$6) }
  | FUN var ARROW expr                  { EFun($2,$4) }
  | bool_expr                           { $1 }
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


