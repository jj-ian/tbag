default: compiler

hello_world: tbagger
	./run.sh hello_world.tbag hello_world.java
 
compiler: scanner parser compile tbagger
	ocamlc -o tbag scanner.cmo parser.cmo compile.cmo tbag.cmo

tbagger:
	ocamlc -c tbag.ml;

compile: 
	ocamlc -c compile.ml

scanner: parser
	ocamllex scanner.mll; ocamlc -o scanner scanner.ml 

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

ast:
	ocamlc -c ast.mli

.PHONY: clean
clean:
	rm -f scanner.ml parser.ml parser.mli *.cmo *.cmi scanner a.out *.cmx scannertraced* *.o *.class tbag *.java
