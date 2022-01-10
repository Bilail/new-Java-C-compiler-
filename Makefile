INTERFACES = Parse.mli
#SOURCES    = ast.ml Parse.ml Lex.ml misc.ml print.ml compil.ml main.ml
SOURCES    = ast.ml Parse.ml Lex.ml  main.ml
GENERATED  = Lex.ml Parse.ml Parse.mli Parse.automaton Parse.conflicts

projet: Parse.mli $(SOURCES)
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o tp $(SOURCES)

testLex : Parse.mli Lex.ml testLex.ml 
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o testLex ast.ml  Parse.ml Lex.ml testLex.ml


Lex.ml: Lex.mll 
	ocamllex Lex.mll
	./testLex ./test/ex1.txtclear 

Parse.mli : Parse.mly ast.ml
	menhir --dump --explain --strict Parse.mly

clean:
	rm -rf  tp testLex *.o *.cmi *.cmo *.cmx *~ $(GENERATED)
