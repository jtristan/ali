#!/bin/bash

export TRANSFORM=`pwd`/example.cmo
export ALILIB=`pwd`/ali.cmo
export OCAMLSTDLIB="/usr/local/lib/ocaml/stdlib.cma"
../../../../Release/bin/opt $1 -load ../../../../Release/lib/Ali.dylib -ali -o /dev/null