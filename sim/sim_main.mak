################################################################################
# common makefile tail
################################################################################
# process goals and defaults

GOALS:=$(MAKECMDGOALS)

# default targets if none specified on command line
ifeq (,$(strip $(GOALS)))
GOALS:=$(DEFAULT_SIM) $(DEFAULT_RUN)
endif

# default simulator
ifneq (,$(filter $(SIM_RUNS) $(SIM_OPTIONS),$(GOALS)))
ifeq (,$(strip $(filter $(SIMULATORS),$(GOALS))))
GOALS:=$(DEFAULT_SIM) $(GOALS)
endif
endif

ifneq (,$(strip $(filter $(SIMULATORS),$(GOALS))))
ifneq (,$(filter gui,$(GOALS)))
# interactive simulation - one simulator, no runs allowed
ifneq (1,$(words $(filter $(SIMULATORS),$(GOALS))))
$(error Only one simulator should be specified for interactive simulation)
endif
ifneq (,$(filter $(SIM_RUNS),$(GOALS)))
$(error Runs should not be specified for interactive simulation)
endif
else
# batch simulation - set default run if none specified
ifeq (,$(filter $(SIM_RUNS),$(GOALS)))
GOALS:=$(GOALS) $(DEFAULT_RUN)
endif
endif
endif

.PHONY: default $(SIMULATORS) $(SIM_RUNS) $(SIM_OPTIONS) $(AUX_TARGETS) force
default: $(GOALS)
force:

################################################################################
# rule/recipe to simulate - with ModelSim/Questa/etc (vsim)

ifneq (,$(filter vsim,$(GOALS)))

sim_vsim/makefile: $(MAKE_FPGA)/make_vsim.py $(this_makefile) $(foreach l,$(SIM_LIBS),$(SIM_SRC.$l)) | sim_vsim
	MSYS2_ARG_CONV_EXCL='--run ' $(PYTHON) $(MAKE_FPGA)/make_vsim.py \
		--path $(VSIM_PATH) \
		$(foreach l,$(SIM_LIBS),--src $(SIM_SRC.$l)=$l) \
		$(foreach r,$(SIM_RUNS),--run $(SIM_RUN.$r)) \
		$(foreach g,$(SIM_GEN),--gen $g) \
		> $@

sim_vsim/vsim.do: $(MAKE_FPGA)/do_vsim.py $(this_makefile) | sim_vsim
	MSYS2_ARG_CONV_EXCL='--run ' $(PYTHON) $< \
		$(foreach l,$(SIM_LIBS),--src $(SIM_SRC.$l)=$l) \
		$(foreach r,$(SIM_RUNS),--run $(SIM_RUN.$r)) \
		$(foreach g,$(SIM_GEN),--gen $g) \
		> $@

gui: sim_vsim/vsim.do force
ifeq ($(OS),Windows_NT)
	cd $(dir $<) && start $(VSIM) -gui -do "do $(notdir $<)"
else
	cd $(dir $<) && $(VSIM) -gui -do "do $(notdir $<)" &
endif

endif

###############################################################################
# rule/recipe to simulate - with NVC

ifneq (,$(filter nvc,$(GOALS)))

sim_nvc/makefile: $(MAKE_FPGA)/make_nvc.py $(this_makefile) $(foreach l,$(SIM_LIBS),$(SIM_SRC.$l)) | sim_nvc
	MSYS2_ARG_CONV_EXCL='--run ' $(PYTHON) $< \
		$(foreach l,$(SIM_LIBS),--src $(SIM_SRC.$l)=$l) \
		$(foreach r,$(SIM_RUNS),--run $(SIM_RUN.$r)) \
		$(foreach g,$(SIM_GEN),--gen $g) \
		> $@

endif

################################################################################
# common rules/recipes

define rr_dir
sim_$1:
	mkdir -p $$@
endef

define rr_run
sim_$1/log_$2.txt: sim_$1/makefile force
	$$(MAKE) $2 -f $$(notdir $$<) -C $$(dir $$<)
	@if [ "$$$$( tail -n 1 $$@ )" = "PASS" ]; then \
		echo -e "\e[42m$$@ : PASS\033[0m"; exit 0; \
	else \
		echo -e " \e[41m$$@ : FAIL\033[0m"; exit 1; \
	fi
$2:: sim_$1/log_$2.txt
endef

$(foreach s,$(filter $(SIMULATORS),$(GOALS)),$(eval $(call rr_dir,$s)))
$(foreach s,$(filter $(SIMULATORS),$(GOALS)),$(foreach r,$(SIM_RUNS),$(eval $(call rr_run,$s,$r))))
$(foreach s,$(filter $(SIMULATORS),$(GOALS)),$(foreach r,$(SIM_RUNS),$(eval $(call rr_post,$s,$r))))

$(SIMULATORS): $(filter $(SIM_RUNS),$(GOALS))

################################################################################
# edit with VSCode/V4P

EDIT_DIR?=vscode_ws

$(EDIT_DIR):
	bash -c "mkdir -p $@"
$(EDIT_DIR)/makefile: $(MAKE_FPGA)/make_vscode.py $(this_makefile) $(foreach l,$(SIM_LIBS),$(SIM_SRC.$l)) | $(EDIT_DIR)
	$(PYTHON) $(MAKE_FPGA)/make_vscode.py \
		$(foreach p,$(PRIM_LIBS),$(if $(findstring $p,$(foreach l,$(SIM_LIBS),$(SIM_SRC.$l))),--src $(PRIM_SRC.$p)=$p)) \
		$(foreach l,$(SIM_LIBS),--src $(SIM_SRC.$l)=$l) \
		--top $(SIM_TB) \
		> $@
vscode:: $(EDIT_DIR)/makefile
	$(MAKE) $@ -f $(notdir $<) -C $(dir $<)

################################################################################
# cleanup

clean::
	rm -f *.vhd
	rm -rf $(addprefix sim_,$(SIMULATORS))
