all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlopt -c parser.mli
	ocamlopt -c lexer.ml
	ocamlopt -c parser.ml
	ocamlopt -c main.ml
	ocamlopt -o calc lexer.cmx parser.cmx main.cmx
