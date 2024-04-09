create_clock -period 12.500 -name clk [get_ports clk]

set_property KEEP_HIERARCHY {TRUE} [get_cells U_DECODER]
set_property KEEP_HIERARCHY {TRUE} [get_cells U_REGFILE]
set_property KEEP_HIERARCHY {TRUE} [get_cells U_ALU]
set_property KEEP_HIERARCHY {TRUE} [get_cells U_CSR]
