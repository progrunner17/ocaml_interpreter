SOURCES = syntax.ml lexer.mll parser.mly  tySyntax.mli tySyntax.ml  constraintSolver.mli constraintSolver.ml infer.ml eval.ml main.ml
RESULT  = my_ocaml
# LIBS	= tySyntax constraintSolver
YFLAGS = -v

all: byte-code byte-code-library

-include OCamlMakefile
