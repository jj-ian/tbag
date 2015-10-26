#!/bin/bash
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c tbagInterpreter.ml
