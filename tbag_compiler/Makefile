default: compiler
 
compiler: scanner parser java_builder tbagger 
	ocamlc -o tbag scanner.cmo parser.cmo java_builder.cmo tbag.cmo

tbagger:
	ocamlc -c tbag.ml;

java_builder: jast
	ocamlc -c java_builder.ml 

scanner: parser
	ocamllex scanner.mll; ocamlc -o scanner scanner.ml 

parser: jast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

jast: sast ast
	ocamlc -c jast.mli

sast: ast
	ocamlc -c sast.mli

ast:
	ocamlc -c ast.mli

.PHONY: clean
clean:
	rm -f scanner.ml parser.ml parser.mli *.cmo *.cmi scanner a.out *.cmx scannertraced* *.o *.class tbag *.java
