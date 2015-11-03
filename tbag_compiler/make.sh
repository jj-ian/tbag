#!/bin/bash

set -x
set -o errexit #script quits on error
set -o pipefail
set -o nounset
# set -o xtrace

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.mli

