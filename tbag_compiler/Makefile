default: runner

runner: tbagger
	./run.sh hello_world.tbag hello_world.java

tbagger: compiler
	ocamlc -c tbag.ml; 

compiler: scanner parser compile
	ocamlc -o tbag scanner.cmo parser.cmo compile.cmo tbag.cmo

compile: 
	ocamlc -c compile.ml

scanner: 
	ocamllex scanner.mll; ocamlc -o scanner scanner.ml 

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

ast:
	ocamlc -c ast.mli