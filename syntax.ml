type name = string
and  env = (name * value) list
and value =
  | VInt  of int
  | VBool of bool
  | VFun  of name * expr * env
  | VRecFun of name * name * expr * env
and expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EAnd       of expr * expr
  | EOr        of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | EFun       of name * expr
  | EApp       of expr * expr
  | ELetRec    of name * name * expr * expr
and command =
  | CExp  of expr
  | CDecl of name * expr
  | CMultiDecl of name * expr * command
  | CAndDecl of name * expr * command
  | CRecDecl of name * name * expr


let print_name = print_string

let print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (_,_,_) -> print_string "<fun>"
  | VRecFun (_,_,_,_) -> print_string "<fun>"

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 を活用すること
*)
let rec  print_expr e =
  match e with
  | EConstInt i ->
     print_int i
  | EConstBool b ->
     print_string (string_of_bool b)
  | EVar x ->
     print_name x
  | EAdd (e1,e2) ->
     (print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ESub (e1,e2) ->
     (print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EMul (e1,e2) ->
     (print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EDiv (e1,e2) ->
     (print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EAnd (e1,e2) ->
     (print_string "EAnd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EOr (e1,e2) ->
     (print_string "EOr (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EEq (e1,e2) ->
     (print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELt (e1, e2) ->
     (print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EIf (e1,e2,e3) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ",";
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | ELet (e1,e2,e3) ->
     (print_string "ELet (";
      print_name   e1;
      print_string ",";
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | EFun (e1,e2) ->
    (print_string "EFun (";
    print_name e1;
    print_string ",";
    print_expr e2;
    print_string ")")
  | EApp (e1,e2) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ",";
      print_expr   e2;
      print_string ")")
  | ELetRec (id,x,e1,e2) ->
     (print_string ("ELetRec (" ^ id ^ "," ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")


let rec print_command p =
  match p with
  | CExp e -> print_expr e
  | CDecl (e1,e2) -> (print_string "val ";
                      print_name e1;
                      print_string ":";
                      print_expr e2)
  | CMultiDecl (e1,e2,e3) -> (print_string "val ";
                      print_name e1;
                      print_string ":";
                      print_expr e2;
                      print_command e3)
  | CAndDecl (e1,e2,e3) -> (print_string "val ";
                      print_name e1;
                      print_string ":";
                      print_expr e2;
                      print_command e3)
  | CRecDecl (id,x,e) ->
     (print_string ("CRecDecl (" ^ id ^ "," ^ x ^ ",");
      print_expr e;
      print_string ")")

