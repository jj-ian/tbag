#!/bin/bash
set -x

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml

set +x
