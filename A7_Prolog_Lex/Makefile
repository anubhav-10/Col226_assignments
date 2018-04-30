all: lexer.cmo parser.cmo main.cmo a6.cmo
	ocamlc -g -o main unix.cma a6.cmo lexer.cmo parser.cmo main.cmo
 
lexer.cmo: lexer.ml parser.cmi
	ocamlc -g -c lexer.ml

parser.cmo: parser.ml parser.cmi
	ocamlc -g -c parser.ml

main.cmo: main.ml
	ocamlc -g -c main.ml

parser.cmi: parser.mli
	ocamlc -g -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.mli: parser.mly a6.cmo
	ocamlyacc parser.mly

parser.ml: parser.mly
	ocamlyacc parser.mly

a6.cmo: a6.ml
	ocamlc -g -c a6.ml

clean: 
	rm -f *.cm*
	rm -f *.mli
	rm -f lexer.ml parser.ml
