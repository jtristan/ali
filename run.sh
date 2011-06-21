#!/bin/bash

export ALI=`pwd`
../../Release/llvm/bin/opt $1 -load ../../Release/llvm/lib/Ali.dylib -caml