type tyvar
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyPair of ty * ty
  | TyList of ty


val new_tyvar : unit -> tyvar
val print_type : ty -> unit
val print_tyvar : tyvar -> unit