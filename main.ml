open Syntax
open Eval

let rec read_eval_print env  =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  try let (id, newenv, v) = eval_command env cmd in
  (Printf.printf "%s : " id;
   print_string " = ";
   print_value v;
   print_newline ();
   read_eval_print newenv
   )
  with (EvalErr str) -> print_string str;print_newline ();(read_eval_print env)

let initial_env = empty_env
let initial_tyenv = empty_env

let _ = read_eval_print initial_env
