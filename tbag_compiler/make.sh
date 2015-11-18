

#!/bin/bash

set -x
set -o errexit #script quits on error
set -o pipefail
set -o nounset
# set -o xtrace



ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.mli
ocamlc -c parser.mli # compile parser types
#ocamlc -c scanner.ml # compile the scanner
ocamlc -o scanner scanner.ml # temp --compile scanner and name executable scanner 
ocamlc -c parser.ml # compile the parser
#ocamlc -c calc.ml # compile the interpreter
#ocamlc -o calc parser.cmo scanner.cmo calc.cmo
ocamlc -c compile.ml
ocamlc -c tbag.ml

ocamlc -o tbag scanner.cmo parser.cmo compile.cmo tbag.cmo
