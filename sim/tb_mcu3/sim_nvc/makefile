# makefile generated by make_nvc.py (see https://github.com/amb5l/make-fpga)
# for simulation using NVC

################################################################################
# simulation specific definitions

# libraries to compile source into
LIB:=work

# sources in compilation order (source=library)
SRC:= \
	D:/work/retro_fpga/jarv/src/rtl/packages/common.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/packages/isa.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/modules/decoder.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/modules/regfile_ad.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/modules/alu.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/modules/csr_m.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/cores/mcu3.vhd=work \
	D:/work/retro_fpga/jarv/src/rtl/test/tb_mcu3.vhd=work

# other simulation prerequisites (e.g. data files)
DEP:=

# simulation runs (top plus any run specific generic/SDF assignments)
RUNS:=run
RUN.run:=tb_mcu3

# generic assignments (applied to all simulation runs)
GEN:=

# global, analysis, elaboration and run options
NVC_GOPTS+=--std=2008 -L.
NVC_AOPTS+=--relaxed
NVC_EOPTS+=
NVC_ROPTS+=--ieee-warnings=off

################################################################################

# default goal
all: nvc

# default name for single run
RUN:=$(if $(word 2,$(RUN)),$(RUN),$(if $(filter :,$(RUN)),$(RUN),sim:$(RUN)))

# useful definitions
comma:=,
rest=$(wordlist 2,$(words $1),$1)
chop=$(wordlist 1,$(words $(call rest,$1)),$1)
src_dep=$1,$2
pairmap=$(and $(strip $2),$(strip $3),$(call $1,$(firstword $2),$(firstword $3)) $(call pairmap,$1,$(call rest,$2),$(call rest,$3)))

# compilation dependencies enforce compilation order
dep:=$(firstword $(SRC)), $(if $(word 2,$(SRC)),$(call pairmap,src_dep,$(call rest,$(SRC)),$(call chop,$(SRC))),)

# generate rule(s) and recipe(s) to compile source(s)
define rr_compile
$1/$(notdir $2).com: $2 $3 $(DEP)
	nvc $(NVC_GOPTS) --work=$(strip $1) -a $(NVC_AOPTS) $$<
	touch $$@
endef
$(foreach d,$(dep),$(eval $(call rr_compile, \
$(word 2,$(subst =, ,$(word 1,$(subst $(comma), ,$d)))), \
$(word 1,$(subst =, ,$(word 1,$(subst $(comma), ,$d)))), \
$(addsuffix .com,$(addprefix $(word 2,$(subst =, ,$(word 2,$(subst $(comma), ,$d))))/,$(notdir $(word 1,$(subst =, ,$(word 2,$(subst $(comma), ,$d))))))) \
)))

# generate rule(s) and recipe(s) to run simulation(s)
define rr_run
$1: $(word 2,$(subst =, ,$(lastword $(SRC))))/$(notdir $(word 1,$(subst =, ,$(lastword $(SRC))))).com
	@bash -c 'echo -e "[0;32mRUN: $1 ($(word 1,$(RUN.$1)))  start at $$$$(date +%T.%2N)[0m"'
	nvc $(NVC_GOPTS) -e $(NVC_EOPTS) $(GEN) $(RUN.$1)
	nvc $(NVC_GOPTS) -r $(NVC_ROPTS) $(word 1,$(RUN.$1))
	@bash -c 'echo -e "[0;31mRUN: $1 ($(word 1,$(RUN.$1)))    end at $$$$(date +%T.%2N)[0m"'
nvc:: $1
endef
$(foreach r,$(RUNS),$(eval $(call rr_run,$r)))
