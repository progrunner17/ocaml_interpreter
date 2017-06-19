open Syntax
open Eval
open Infer

let rec read_eval_print env tyenv  =
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  try
  let (t,newtyenv)    = infer_command tyenv cmd in
  let (id, newenv, v) = eval_command env cmd in
  (Printf.printf "%s : " id;
   print_string " = ";
   print_value v;
   print_newline ();
   read_eval_print newenv newtyenv
   )
  with
  |InferErr -> print_string "InferErr" ;print_newline ();(read_eval_print env tyenv)
  |EvalErr str -> print_string "EvarErr" ;print_string str;print_newline ();(read_eval_print env tyenv)


let initial_env = empty_env
let initial_tyenv = empty_env

let _ = read_eval_print initial_env initial_tyenv
