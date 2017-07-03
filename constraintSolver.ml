open TySyntax
exception TyError of string
type subst = (tyvar * ty) list
type constraints = (ty * ty) list


let  print_const const =
	let rec sub_print_const sub_const =
	match sub_const with
	|[] -> ()
	|(t1,t2)::const' -> print_string "(";
					TySyntax.print_type t1;
					print_string ",";
					TySyntax.print_type t2;
					print_string ");";
					sub_print_const const';
	in
	print_string "[";
	sub_print_const const;
	print_string "]"

let  print_subst subst =
	let rec sub_print_subst sub_subst =
	match sub_subst with
	|[] -> ()
	|(t1,t2)::subst' -> print_string "(";
					TySyntax.print_type (TyVar(t1));
					print_string ",";
					TySyntax.print_type t2;
					print_string ");";
					sub_print_subst subst'
	in
	print_string "[";
	sub_print_subst subst;
	print_string "]"


(*
** @func 	lookup_ty
** @arg 	tyvar
** @return 	ty
** @comment なければそのまま返す
*)
let rec lookup_ty subst tyvar =
	match subst with
	| [] -> TyVar(tyvar)
	| (tyvar_key,t)::subst'-> if tyvar = tyvar_key then t else (lookup_ty subst' tyvar)

let rec update_ty old_ty new_ty ty=
	match ty with
	| x  when x = old_ty -> new_ty
	| TyFun(t1,t2) -> TyFun((update_ty old_ty new_ty t1),(update_ty old_ty new_ty t2))
	| _ ->ty

let rec update_subst old_ty new_ty subst =
	match subst with
	|[] -> []
	|(t1,t2)::constraints' ->((t1,(update_ty old_ty new_ty t2))::(update_subst old_ty new_ty constraints'))



	(* f(g(x))を計算 *)
let rec compose subst_f subst_g =
(* 	print_string "compose ";
	print_subst subst_f;
	print_string "○";
	print_subst subst_g;
	print_newline (); *)
	match subst_g with
	|[] -> subst_f
	|(tyvar,ty)::subst-> match ty with
		|TyInt | TyBool | TyFun(_,_) -> (tyvar,ty)::(compose (update_subst (TyVar(tyvar)) ty subst_f) (update_subst (TyVar(tyvar)) ty subst))
		|TyVar(tyvarf) ->(tyvar,(update_ty (TyVar(tyvar)) ty (lookup_ty subst_f tyvarf)))::(compose (update_subst (TyVar(tyvar)) ty subst_f) subst)



let rec occurence_detect a ty =
	match ty with
	|TyVar(i) when i = a -> print_tyvar i;raise (TyError "occurence" )
	|TyFun(t1,t2) -> TyFun((occurence_detect a t1),(occurence_detect a t2))
	|_ ->(* 
	print_string "detect " ;
	print_type (TyVar(a));
	print_string " in ";
	print_type ty;
	print_newline (); *)ty


let rec update old_ty new_ty constraints =
(* 	print_string "update ";
		print_type old_ty;
		print_string " to ";
		print_type new_ty;
		print_string " in ";
		print_const constraints;
		print_newline (); *)
	match constraints with
	|[] -> []
	|(t1,t2)::constraints' ->(((update_ty old_ty new_ty t1),(update_ty old_ty new_ty t2))::(update old_ty new_ty constraints'))


let rec unify constraints =
	match constraints with
	|[] -> []
	|(ty1,ty2)::constraints'->(match ty1,ty2 with
		| TyInt,TyInt | TyBool,TyBool -> (unify constraints')
		| (TyFun(t11,t12),TyFun(t21,t22)) -> (unify ([(t11,t21);(t12,t22)] @ constraints'))
		| (ty,TyVar(tyvar))| (TyVar(tyvar),ty)  ->
				if (TyVar(tyvar)) = ty then (unify constraints') else
				(let tty = (occurence_detect  tyvar ty) in (compose [(tyvar,tty)] (unify (update (TyVar(tyvar)) tty constraints')) ))
		| _,_ ->raise (TyError "match error") )


(*
** @func 	ty_subst
** @arg 	subst
** @arg		ty
** @return 	ty
** @comment tyをsubstitute
*)

let rec ty_subst subst ty =
	match ty with
	| TyInt -> TyInt
	| TyBool -> TyBool
	| TyFun(t1,t2) -> (* print_string "subst ";
					 print_type ty;
					 print_string " to ";
					 print_type (TyFun((ty_subst subst t1),(ty_subst subst t2)));
					 print_newline (); *)
					 TyFun((ty_subst subst t1),(ty_subst subst t2))
	| TyVar(tyvar) -> (* print_string "subst";
					 print_type ty;
					 print_string " to ";
					 print_type (lookup_ty subst tyvar);
					 print_newline (); *)
					 (lookup_ty subst tyvar)




