#
# BYTE CODE
#
.PHONY: all
all: lib-matrix lib-pgm lib-neural_net lib-init
	@cd encoder && $(MAKE)
	@cd decoder && $(MAKE)
#
# LIBS
#
.PHONY: lib-matrix
lib-matrix:
	@cd matrix && $(MAKE) byte-code-library

.PHONY: lib-pgm
lib-pgm:
	@cd pgm && $(MAKE) byte-code-library

.PHONY: lib-neural_net
lib-neural_net:
	@cd neural_net && $(MAKE) byte-code-library

.PHONY: lib-init
lib-init:
	@cd init && $(MAKE) byte-code-library
#
# NATIVE CODE
#
.PHONY: exe-opt
exe-opt: lib-matrix-opt lib-pgm-opt lib-neural_net-opt lib-init-opt
	@cd encoder && $(MAKE) nc
	@cd decoder && $(MAKE) nc


bc: all
nc: exe-opt

.PHONY: lib-matrix-opt
lib-matrix-opt:
	@cd matrix && $(MAKE) native-code-library

.PHONY: lib-pgm-opt
lib-pgm-opt:
	@cd pgm && $(MAKE) native-code-library

.PHONY: lib-neural_net-opt
lib-neural_net-opt:
	@cd neural_net && $(MAKE) native-code-library

.PHONY: lib-init-opt
lib-init-opt:
	@cd init && $(MAKE) native-code-library

#
# CLEAN
#
.PHONY:	clean
clean:
	@cd matrix && $(MAKE) clean
	@cd pgm && $(MAKE) clean
	@cd neural_net && $(MAKE) clean
	@cd init && $(MAKE) clean
	@cd encoder && $(MAKE) clean
	@cd decoder && $(MAKE) clean
