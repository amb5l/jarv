include ../sim_head.mak

SIMULATORS:=vsim nvc
SIM_OPTIONS:=gui
AUX_TARGETS:=vscode clean
DEFAULT_SIM:=vsim
DEFAULT_RUN:=run

include ../sim_goal.mak

DUT:=mcu3
SIM_TB:=tb_$(DUT)
SIM_LIBS:=work
SIM_SRC.work:=\
	$(SRC)/packages/common.vhd \
	$(SRC)/packages/isa.vhd \
	$(SRC)/modules/decoder.vhd \
	$(SRC)/modules/regfile_ad.vhd \
	$(SRC)/modules/alu.vhd \
	$(SRC)/modules/csr_m.vhd \
	$(SRC)/cores/$(DUT).vhd \
	$(SRC)/test/$(SIM_TB).vhd
SIM_RUNS:=run
SIM_RUN.run:=run:$(SIM_TB)

include ../sim_main.mak
