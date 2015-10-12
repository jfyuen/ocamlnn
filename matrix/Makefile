OCAMLMAKEFILE = ../OCamlMakefile

OCAMLNLDFLAGS = -unsafe -noassert
OCAMLNCFLAGS = -unsafe -noassert

LIBS = str

SOURCES = element_intf.mli element.mli element.ml \
	  array_matrix.mli array_matrix.ml \
	  matrix_intf.mli matrix.mli matrix.ml
RESULT  = matrix

LIBINSTALL_FILES = matrix.cmi matrix.cma matrix.cmxa matrix.a

all:

install:	libinstall
uninstall:	libuninstall

-include $(OCAMLMAKEFILE)
