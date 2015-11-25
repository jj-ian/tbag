default: compiler
 
compiler: scanner parser c_builder pretty_printer tbagger 
	ocamlc -o tbag scanner.cmo parser.cmo c_builder.cmo pretty_printer.cmo tbag.cmo

tbagger:
	ocamlc -c tbag.ml;

pretty_printer:
	ocamlc -c pretty_printer.ml

c_builder: cast
	ocamlc -c c_builder.ml 

scanner: parser
	ocamllex scanner.mll; ocamlc -o scanner scanner.ml 

parser: cast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

cast: sast ast
	ocamlc -c cast.mli

sast: ast
	ocamlc -c sast.mli

ast:
	ocamlc -c ast.mli

.PHONY: clean
clean:
	rm -f scanner.ml parser.ml parser.mli *.cmo *.cmi scanner a.out *.cmx scannertraced* *.o *.class tbag *.java
