OCAMLMAKEFILE = ../OCamlMakefile

OCAMLNLDFLAGS = -unsafe -noassert
OCAMLNCFLAGS = -unsafe -noassert

INCDIRS = ../matrix
LIBS = str matrix

SOURCES = pgm.mli pgm.ml
RESULT  = pgm

LIBINSTALL_FILES = pgm.cmi pgm.cma pgm.cmxa pgm.a

all: 

install:	libinstall
uninstall:	libuninstall

-include $(OCAMLMAKEFILE)
