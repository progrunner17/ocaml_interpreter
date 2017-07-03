type tyvar = int

type  ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyPair of ty * ty
  | TyList of ty

let typegenerator = ref (0 - 1)

let new_tyvar () = typegenerator:= !typegenerator + 1; !typegenerator



let rec print_type t =
		match t with
		| TyInt -> print_string "int"
		| TyBool -> print_string "bool"
		| TyFun(t1,t2)  -> print_string "(" ;print_type t1; print_string " -> "; print_type t2 ; print_string ")"
		| TyVar(x) -> print_string ("a" ^ (string_of_int x))
		| TyPair(t1,t2) -> print_type t1; print_string "*"; print_type t2
		| TyList(t) -> print_type t ; print_string " list"
let  print_tyvar = print_int


