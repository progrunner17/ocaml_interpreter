open Syntax
open Eval
open Infer

let rec read_eval_print env tyenv  =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (id, newenv, v) = eval_command env tyenv cmd in
  (Printf.printf "%s : " id;
   (* print_type ty; *)
   print_string " = ";
   print_value v;
   print_newline ();
   read_eval_print newenv)

let initial_env = empty_env
let initial_tyenv = empty_env

let _ = read_eval_print initial_env initial_tyenv
