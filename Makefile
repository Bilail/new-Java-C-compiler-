INTERFACES = Parse.mli
#SOURCES    = ast.ml Parse.ml Lex.ml misc.ml print.ml compil.ml main.ml
SOURCES    = ast.ml StringAndInteger.ml Parse.ml Lex.ml print.ml ContextAnalysisTools.ml ContextAnalysis.ml main.ml
#SOURCES    = ast.ml Parse.ml Lex.ml ContextAnalysis.ml main.ml
GENERATED  = Lex.ml Parse.ml Parse.mli Parse.automaton Parse.conflicts

projet: Parse.mli $(SOURCES)
	clear
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o awesomeCompiler $(SOURCES)
	./awesomeCompiler ./test/ex1-V3.txt
	

testLex : Parse.mli Lex.ml testLex.ml 
	ocamlc -c ast.ml
	ocamlc $(INTERFACES)
	ocamlc -o testLex ast.ml  Parse.ml Lex.ml testLex.ml


Lex.ml: Lex.mll 
	ocamllex Lex.mll

Parse.mli : Parse.mly ast.ml
	menhir --dump --explain --strict Parse.mly

clean:
	rm -rf  awesomeCompiler testLex *.o *.cmi *.cmo *.cmx *~ $(GENERATED)
