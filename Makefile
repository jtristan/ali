# Makefile for caml pass

LDFLAGS+= -L/usr/local/lib/ocaml/ -lcamlrun

# Path to top level of LLVM hierarchy
LEVEL = ../../..

# Name of the library to build
LIBRARYNAME = Ali

# Make the shared library become a loadable module so the tools can 
# dlopen/dlsym on the resulting library.
LOADABLE_MODULE = 1

# Include the makefile implementation stuff
include $(LEVEL)/Makefile.common

plugin: plugin.ml
	ocamlc -output-obj -o plugin.c dynlink.cma stdlib.cma plugin.ml

aliml: ali.ml
	ocamlc -c ali.ml

example: 
	ocamlc -c ali.cmo example.ml -o example.cmo