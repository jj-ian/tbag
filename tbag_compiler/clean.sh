#!/bin/bash

set -x #prints out commands executed
set -o errexit #script quits on error
set -o pipefail
set -o nounset
# set -o xtrace

rm -f scanner.ml parser.ml parser.mli *.cmo *.cmi scanner a.out *.cmx scannertraced* *.o

