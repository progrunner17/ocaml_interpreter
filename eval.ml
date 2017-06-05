open Syntax

exception Unbound

(* type env = (name * value) list
 *)
let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

 exception EvalErr of string

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
       lookup x env
     with
     | Unbound -> raise (EvalErr ("Unbound value " ^ x)))
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise (EvalErr"argments has differlent type"))
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 / i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | EAnd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool i1, VBool i2 -> VBool (i1 && i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | EOr (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool i1, VBool i2 -> VBool (i1 || i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | VBool i1,  VBool i2  -> VBool (i1 = i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | VBool i1,  VBool i2  -> VBool (i1 < i2)
     | _ -> raise (EvalErr "argments has differlent type"))
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> raise (EvalErr ("condition has type " ^ "int " ^ "but an expression was expected of type bool ")))
  | ELet (e1,e2,e3) ->
    (eval_expr ((e1,(eval_expr env e2))::env)  e3)
  |EFun  (x,e)  -> VFun (x,e,env)
  |EApp  (e1,e2) ->
    let v1  = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
    | VFun(x,e,oenv) ->
      eval_expr (extend x v2 oenv) e
    | VRecFun(f,x,e,oenv) ->
      let env' =
        extend x v2  (extend f (VRecFun(f,x,e,oenv)) oenv)
        in
          eval_expr env' e
    | _ -> raise (EvalErr "not function"))
    | ELetRec(f,x,e1,e2) ->
        let env' =
          extend f (VRecFun(f,x,e1,env)) env
          in
            eval_expr env' e2

let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1,e2) -> (e1,((e1,(eval_expr env e2))::env),(eval_expr env e2))
  | CMultiDecl(x,e,next) -> (Printf.printf "%s = " x;
                            print_value (eval_expr env e);
                            print_newline ();
                         let v = eval_expr env e in (eval_command (extend x v env) next))
  | CAndDecl (x,e,next) ->(Printf.printf "%s = " x;
                            print_value (eval_expr env e);
                            print_newline ();
                          let (y,newenv,vy) = eval_command env next in
                          let vx = eval_expr env e in (y,(extend x vx newenv),vy))
  | CRecDecl (f,x,e)   -> (f,(extend f (VRecFun(f,x,e,env)) env),VRecFun(f,x,e,env))


