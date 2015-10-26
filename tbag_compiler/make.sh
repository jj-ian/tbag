#!/bin/bash
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
