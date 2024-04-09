################################################################################
# common makefile header
################################################################################

rest=$(wordlist 2,$(words $1),$1)
chop=$(wordlist 1,$(words $(call rest,$1)),$1)

this_makefile:=$(lastword $(call chop,$(MAKEFILE_LIST)))
include $(shell realpath --relative-to . $(shell git rev-parse --show-toplevel))/../make-fpga/start.mak
this_makefile:=$(call xabspath,$(this_makefile))
MAKE_FPGA:=$(REPO_ROOT)/../make-fpga

LATTICE_RADIANT:=$(call xabspath,$(LATTICE_RADIANT))
VSIM_PATH:=$(call xabspath,$(LATTICE_RADIANT)/modeltech/$(if $(filter Windows_NT,$(OS)),win32loem,linuxloem))
VSIM:=$(call xabspath,$(VSIM_PATH)/vsim)
PYTHON=python
SRC=$(REPO_ROOT)/src/rtl
