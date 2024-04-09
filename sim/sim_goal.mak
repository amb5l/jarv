################################################################################
# common goal handling
################################################################################

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
