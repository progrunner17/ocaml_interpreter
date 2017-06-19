open Syntax

exception Unbound

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
      (eval_expr (extend x v2 oenv) e)
    | VRecFun(f,x,e,oenv)  ->(
          let oenv' = extend f v1 oenv in
      eval_expr (extend x v2 oenv') e)
    | _ -> raise (EvalErr "not function"))
  | ELetRec(f,x,e1,e2) ->(
          let env' = extend f (VRecFun(f,x,e,env)) env
          in
            eval_expr env' e2)


let rec eval_command env c =
  match c with
  | CExp e ->
        let v = eval_expr env e in
        ("-", env, v)
  | CDecl (x,e) ->
        let v = eval_expr env e in
        ("val " ^ x, (extend x v env), v)
  | CRecDecl (f,x,e)   ->
        let v = VRecFun(f,x,e,env) in
        ("val " ^ f, (extend f v env), v)
