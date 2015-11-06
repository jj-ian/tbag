#!/bin/bash

set -x
menhir --interpret --interpret-show-cst parser.mly

set +x

