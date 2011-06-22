#!/bin/bash

export ALI=`pwd`/example.cmo
echo Loading $ALI
../../../Release/bin/opt $1 -load ../../../Release/lib/Ali.dylib -ali