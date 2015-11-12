default: tbagger

tbagger: compiler
	ocamlc -c tbag.ml; 

compiler: scanner parser compile
	ocamlc -o tbag scanner.cmo parser.cmo compile.cmo tbag.cmo

compile: 
	ocamlc -c compile.ml

scanner: 
	ocamlc -o scanner scanner.ml 

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

ast:
	ocamlc -c ast.mli