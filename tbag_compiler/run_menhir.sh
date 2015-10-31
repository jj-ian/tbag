#!/bin/bash

set -x
menhir --interpret --interpret-show-cst parser.mly | echo "ID"

set +x

