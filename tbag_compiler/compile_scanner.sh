#!/usr/bin/env bash

# compiles scanner.mll into executable called scanner

set -x
set -o errexit #script quits on error
set -o pipefail
set -o nounset
# set -o xtrace

ocamllex scanner.mll
ocamlc -o scanner scanner.ml