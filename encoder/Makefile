OCAMLMAKEFILE = ../OCamlMakefile

OCAMLNLDFLAGS = -unsafe -noassert
OCAMLNCFLAGS = -unsafe -noassert

INCDIRS = ../matrix ../init ../neural_net ../pgm
LIBS   = str matrix init neural_net pgm

SOURCES = read_conf.mli read_conf.ml \
	  encoder.ml 
RESULT  = encoder

-include $(OCAMLMAKEFILE)
