#!/bin/bash

ocamllex scanner.mll
ocamlc -o execAst ast.ml
