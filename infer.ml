type tyenv = name * ty list
let rec infer_expr tyenv e =　(*reurn ty * const*)
  match e with
  | EConstInt i ->
    (TyInt,[])
  | EConstBool b ->
    (TyBool,[])
  | EVar x ->
    (try
       ((lookup x tyenv),[])
     with
     | (Unbound,_) -> raise (InfErr ("Unbound type " ^ x)))
  | EAdd (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2))
  | ESub (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2))
  | EMul (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2))
  | EDiv (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2))
  | EAnd (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyBool,[(t1,TyBool);(t2,TyBool)]@c1@c2))
  | EAdd (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyBool,[(t1,TyBool);(t2,TyBool)]@c1@c2))
  | EEq (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyBool,[(t1,t2)]@c1@c2))
  | ELt (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
      (TyBool,[(t1,t2)]@c1@c2))
  | EIf (e1,e2,e3) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    let (t3,c3) = infer_expr tyenv e3 in
      (t2,[(t1,TyBool);(t2,t3)]@c1@c2@c3))
  | ELet (x,e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr ((x,t1)::tyenv) e2 in
      (t2,c1@c2))
  | EFun    (x,e)     ->
    let a = TyVar(new_tyvar ()) in
    let (t,c) = infer_expr ((x,a)::tyenv) e in
      (TyFun(a,t),c)
  | EApp (e1,e2) ->(
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    let a = TyVar(new_tyvar ()) in
      (a,[(t1,TyFun(t2,a))]@c1@c2))
  | ELetRec (f,x,e1,e2)   ->
    let a = TyVar(new_tyvar ()) in
    let b = TyVar(new_tyvar ()) in
    let (t1,c1) = infer_expr ([(f,TyFun(a,b));(x,a)]@tyenv) e1 in
    let (t2,c2) = infer_expr ((f,TyFun(a,b))::tyenv) e2 in
      (t2,(t1,b)::c1@c2)



let rec infer_cmd tyenv cmd =
  match cmd with
  |CExp e |CDecl (x,e) ->
        let (t,c) = infer_expr tyenv e in
        let solved_t = (ty_subst (unify c) t) in
        (solved_t,(extend x solved_t))

  |CRecDecl (f,x,e) ->
        let a = TyVar(new_tyvar ()) in
        let b = TyVar(new_tyvar ()) in
        let (t,c) = infer_expr ( [(f,TyFun(a,b));(x,a)] @ tyenv) e in
        let solved_t = (ty_subst (unify c) t) in
        (solved_t,(extend x solved_t))
