SOURCES = syntax.ml lexer.mll parser.mly infer.ml eval.ml main.ml
RESULT  = my_ocaml
LIBS	= tySyntax constraintSolver
YFLAGS = -v

all: byte-code byte-code-library

-include OCamlMakefile
