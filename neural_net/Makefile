OCAMLMAKEFILE = ../OCamlMakefile

OCAMLNLDFLAGS = -unsafe -noassert
OCAMLNCFLAGS = -unsafe -noassert

INCDIRS = ../matrix
LIBS = matrix

SOURCES = neuron.mli neuron.ml \
	  backprop.mli backprop.ml \
	  model.mli \
	  neural_net.mli neural_net.ml
RESULT  = neural_net

LIBINSTALL_FILES = neural_net.cmi neural_net.cma \
		   neural_net.cmxa neural_net.a

all:

install:	libinstall
uninstall:	libuninstall

-include $(OCAMLMAKEFILE)
