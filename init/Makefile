OCAMLMAKEFILE = ../OCamlMakefile

INCDIRS = ../matrix
LIBS = matrix

SOURCES = misc.ml acp.ml gha.ml apex.ml ala.ml rand.ml \
	  init_intf.mli init.mli init.ml
RESULT  = init

LIBINSTALL_FILES = init.cmi init.cma init.cmxa init.a

all:

install:	libinstall
uninstall:	libuninstall

-include $(OCAMLMAKEFILE)
