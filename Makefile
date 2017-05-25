SOURCES = syntax.ml lexer.mll parser.mly eval.ml main.ml
RESULT  = my_ocaml

YFLAGS = -v

all: byte-code byte-code-library

-include OCamlMakefile
