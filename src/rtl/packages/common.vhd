--------------------------------------------------------------------------------
-- common.vhd                                                                 --
--------------------------------------------------------------------------------
-- (C) Copyright 2024 Adam Barnes <ambarnes@gmail.com>                        --
-- This file is part of JARV. JARV is free software: you can redistribute it  --
-- and/or modify it under the terms of the GNU Lesser General Public License  --
-- as published by the Free Software Foundation, either version 3 of the      --
-- License or (at your option) any later version.                             --
-- JARV is distributed in the hope that it will be useful but WITHOUT ANY     --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS  --
-- FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for    --
-- more details. You should have received a copy of the GNU Lesser General    --
-- Public License along with JARV. If not see https://www.gnu.org/licenses/.  --
--------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;

package common_pkg is

  subtype tag_t     is std_ulogic_vector(  3 downto 0 ); -- instruction tag
  subtype inst_t    is std_ulogic_vector( 31 downto 0 ); -- instruction
  subtype opvec_t   is std_ulogic_vector(  4 downto 0 ); -- opcode vector
  subtype xsel_t    is std_ulogic_vector(  4 downto 0 ); -- register select
  subtype f3_t      is std_ulogic_vector(  2 downto 0 );
  subtype f7_t      is std_ulogic_vector(  6 downto 0 );
  subtype f12_t     is std_ulogic_vector( 11 downto 0 );
  subtype imm12_t   is std_ulogic_vector( 11 downto 0 );
  subtype imm20_t   is std_ulogic_vector( 19 downto 0 );
  subtype csr_wop_t is std_ulogic_vector(  1 downto 0 );
  subtype csra_t    is std_ulogic_vector( 11 downto 0 );
  subtype sz_t      is std_ulogic_vector(  1 downto 0 ); -- size (byte, hword, word, dword)
  subtype ssz_t     is std_ulogic_vector(  2 downto 0 ); -- signed size (msb: 1 = unsigned)
  subtype tval_t    is std_ulogic_vector( 63 downto 0 );

  constant SZ_B  : sz_t := "00";
  constant SZ_H  : sz_t := "01";
  constant SZ_W  : sz_t := "10";
  constant SZ_D  : sz_t := "11";

  --------------------------------------------------------------------------------
  -- exceptions

  type exceps_t is record
    interrupt_s_u      : std_ulogic;
    interrupt_s_s      : std_ulogic;
    interrupt_s_h      : std_ulogic;
    interrupt_s_m      : std_ulogic;
    interrupt_t_u      : std_ulogic;
    interrupt_t_s      : std_ulogic;
    interrupt_t_h      : std_ulogic;
    interrupt_t_m      : std_ulogic;
    interrupt_e_u      : std_ulogic;
    interrupt_e_s      : std_ulogic;
    interrupt_e_h      : std_ulogic;
    interrupt_e_m      : std_ulogic;
    interrupt_rsvd_12  : std_ulogic;
    interrupt_rsvd_13  : std_ulogic;
    interrupt_rsvd_14  : std_ulogic;
    interrupt_rsvd_15  : std_ulogic;
    trap_align_instr   : std_ulogic;
    trap_access_instr  : std_ulogic;
    trap_illegal_instr : std_ulogic;
    trap_break         : std_ulogic;
    trap_align_load    : std_ulogic;
    trap_access_load   : std_ulogic;
    trap_align_store   : std_ulogic;
    trap_access_store  : std_ulogic;
    trap_ecall_u       : std_ulogic;
    trap_ecall_s       : std_ulogic;
    trap_ecall_h       : std_ulogic;
    trap_ecall_m       : std_ulogic;
    trap_page_instr    : std_ulogic;
    trap_page_load     : std_ulogic;
    trap_rsvd_14       : std_ulogic;
    trap_page_store    : std_ulogic;
  end record exceps_t;

  constant ECAUSE_INTERRUPT_S_U      : std_ulogic_vector(3 downto 0) := x"0"; -- user software interrupt (deprecated)
  constant ECAUSE_INTERRUPT_S_S      : std_ulogic_vector(3 downto 0) := x"1"; -- supervisor software interrupt
  constant ECAUSE_INTERRUPT_S_H      : std_ulogic_vector(3 downto 0) := x"2"; -- hypervisor software interrupt (deprecated)
  constant ECAUSE_INTERRUPT_S_M      : std_ulogic_vector(3 downto 0) := x"3"; -- machine software interrupt
  constant ECAUSE_INTERRUPT_T_U      : std_ulogic_vector(3 downto 0) := x"4"; -- user timer interrupt (deprecated)
  constant ECAUSE_INTERRUPT_T_S      : std_ulogic_vector(3 downto 0) := x"5"; -- supervisor timer interrupt
  constant ECAUSE_INTERRUPT_T_H      : std_ulogic_vector(3 downto 0) := x"6"; -- hypervisor timer interrupt (deprecated)
  constant ECAUSE_INTERRUPT_T_M      : std_ulogic_vector(3 downto 0) := x"7"; -- machine timer interrupt
  constant ECAUSE_INTERRUPT_E_U      : std_ulogic_vector(3 downto 0) := x"8"; -- user external interrupt (deprecated)
  constant ECAUSE_INTERRUPT_E_S      : std_ulogic_vector(3 downto 0) := x"9"; -- supervisor external interrupt
  constant ECAUSE_INTERRUPT_E_H      : std_ulogic_vector(3 downto 0) := x"A"; -- hypervisor external interrupt (deprecated)
  constant ECAUSE_INTERRUPT_E_M      : std_ulogic_vector(3 downto 0) := x"B"; -- machine external interrupt
  constant ECAUSE_INTERRUPT_RSVD_12  : std_ulogic_vector(3 downto 0) := x"C"; -- reserved
  constant ECAUSE_INTERRUPT_RSVD_13  : std_ulogic_vector(3 downto 0) := x"D"; -- reserved
  constant ECAUSE_INTERRUPT_RSVD_14  : std_ulogic_vector(3 downto 0) := x"E"; -- reserved
  constant ECAUSE_INTERRUPT_RSVD_15  : std_ulogic_vector(3 downto 0) := x"F"; -- reserved
  constant ECAUSE_TRAP_ALIGN_INSTR   : std_ulogic_vector(3 downto 0) := x"0"; -- Instruction address misaligned
  constant ECAUSE_TRAP_ACCESS_INSTR  : std_ulogic_vector(3 downto 0) := x"1"; -- Instruction access fault
  constant ECAUSE_TRAP_ILLEGAL_INSTR : std_ulogic_vector(3 downto 0) := x"2"; -- Illegal instruction
  constant ECAUSE_TRAP_BREAK         : std_ulogic_vector(3 downto 0) := x"3"; -- Breakpoint
  constant ECAUSE_TRAP_ALIGN_LOAD    : std_ulogic_vector(3 downto 0) := x"4"; -- Load address misaligned
  constant ECAUSE_TRAP_ACCESS_LOAD   : std_ulogic_vector(3 downto 0) := x"5"; -- Load access fault
  constant ECAUSE_TRAP_ALIGN_STORE   : std_ulogic_vector(3 downto 0) := x"6"; -- Store/AMO address misaligned
  constant ECAUSE_TRAP_ACCESS_STORE  : std_ulogic_vector(3 downto 0) := x"7"; -- Store/AMO access fault
  constant ECAUSE_TRAP_ECALL_U       : std_ulogic_vector(3 downto 0) := x"8"; -- Environment call from U-mode
  constant ECAUSE_TRAP_ECALL_S       : std_ulogic_vector(3 downto 0) := x"9"; -- Environment call from S-mode
  constant ECAUSE_TRAP_ECALL_H       : std_ulogic_vector(3 downto 0) := x"A"; -- Environment call from H-mode (deprecated)
  constant ECAUSE_TRAP_ECALL_M       : std_ulogic_vector(3 downto 0) := x"B"; -- Environment call from M-mode
  constant ECAUSE_TRAP_PAGE_INSTR    : std_ulogic_vector(3 downto 0) := x"C"; -- Instruction page fault
  constant ECAUSE_TRAP_PAGE_LOAD     : std_ulogic_vector(3 downto 0) := x"D"; -- Load page fault
  constant ECAUSE_TRAP_RSVD_14       : std_ulogic_vector(3 downto 0) := x"E"; -- reserved
  constant ECAUSE_TRAP_PAGE_STORE    : std_ulogic_vector(3 downto 0) := x"F"; -- Store/AMO page fault

  --------------------------------------------------------------------------------
  -- CSR write operations

  constant CSR_WOP_NOP : csr_wop_t := "00"; -- no operation
  constant CSR_WOP_SET : csr_wop_t := "01"; -- set bits
  constant CSR_WOP_CLR : csr_wop_t := "10"; -- clear bits
  constant CSR_WOP_WR  : csr_wop_t := "11"; -- write

  --------------------------------------------------------------------------------
  -- CSR addresses

  -- unprivileged
  constant CSRA_FFLAGS              : csra_t := x"001"; --      RW   Floating-Point Accrued Exceptions
  constant CSRA_FRM                 : csra_t := x"002"; --      RW   Floating-Point Dynamic Rounding Mode
  constant CSRA_FCSR                : csra_t := x"003"; --      RW   Floating-Point Control and Status Register (frm + fflags)
  constant CSRA_CYCLE               : csra_t := x"C00"; -- all  RO   Cycle counter for RDCYCLE instruction
  constant CSRA_TIME                : csra_t := x"C01"; -- all  RO   Timer for RDTIME instruction
  constant CSRA_INSTRET             : csra_t := x"C02"; -- all  RO   Instructions-retired counter for RDINSTRET instruction
  constant CSRA_HPMCOUNTER3         : csra_t := x"C03"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER4         : csra_t := x"C04"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER5         : csra_t := x"C05"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER6         : csra_t := x"C06"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER7         : csra_t := x"C07"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER8         : csra_t := x"C08"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER9         : csra_t := x"C09"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER10        : csra_t := x"C0A"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER11        : csra_t := x"C0B"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER12        : csra_t := x"C0C"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER13        : csra_t := x"C0D"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER14        : csra_t := x"C0E"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER15        : csra_t := x"C0F"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER16        : csra_t := x"C10"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER17        : csra_t := x"C11"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER18        : csra_t := x"C12"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER19        : csra_t := x"C13"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER20        : csra_t := x"C14"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER21        : csra_t := x"C15"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER22        : csra_t := x"C16"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER23        : csra_t := x"C17"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER24        : csra_t := x"C18"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER25        : csra_t := x"C19"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER26        : csra_t := x"C1A"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER27        : csra_t := x"C1B"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER28        : csra_t := x"C1C"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER29        : csra_t := x"C1D"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER30        : csra_t := x"C1E"; --      RO   Performance-monitoring counter
  constant CSRA_HPMCOUNTER31        : csra_t := x"C1F"; --      RO   Performance-monitoring counter
  constant CSRA_CYCLEH              : csra_t := x"C80"; -- all  RO   Upper 32 bits of cycle, RV32 only
  constant CSRA_TIMEH               : csra_t := x"C81"; -- all  RO   Upper 32 bits of time, RV32 only
  constant CSRA_INSTRETH            : csra_t := x"C82"; -- all  RO   Upper 32 bits of instret, RV32 only
  constant CSRA_HPMCOUNTER3H        : csra_t := x"C83"; --      RO   Upper 32 bits of hpmcounter3, RV32 only
  constant CSRA_HPMCOUNTER4H        : csra_t := x"C84"; --      RO   Upper 32 bits of hpmcounter4, RV32 only
  constant CSRA_HPMCOUNTER5H        : csra_t := x"C85"; --      RW   Upper 32 bits of hpmcounter5, RV32 only
  constant CSRA_HPMCOUNTER6H        : csra_t := x"C86"; --      RW   Upper 32 bits of hpmcounter6, RV32 only
  constant CSRA_HPMCOUNTER7H        : csra_t := x"C87"; --      RW   Upper 32 bits of hpmcounter7, RV32 only
  constant CSRA_HPMCOUNTER8H        : csra_t := x"C88"; --      RW   Upper 32 bits of hpmcounter8, RV32 only
  constant CSRA_HPMCOUNTER9H        : csra_t := x"C89"; --      RW   Upper 32 bits of hpmcounter9, RV32 only
  constant CSRA_HPMCOUNTER10H       : csra_t := x"C8A"; --      RW   Upper 32 bits of hpmcounter10, RV32 only
  constant CSRA_HPMCOUNTER11H       : csra_t := x"C8B"; --      RW   Upper 32 bits of hpmcounter11, RV32 only
  constant CSRA_HPMCOUNTER12H       : csra_t := x"C8C"; --      RW   Upper 32 bits of hpmcounter12, RV32 only
  constant CSRA_HPMCOUNTER13H       : csra_t := x"C8D"; --      RW   Upper 32 bits of hpmcounter13, RV32 only
  constant CSRA_HPMCOUNTER14H       : csra_t := x"C8E"; --      RW   Upper 32 bits of hpmcounter14, RV32 only
  constant CSRA_HPMCOUNTER15H       : csra_t := x"C8F"; --      RW   Upper 32 bits of hpmcounter15, RV32 only
  constant CSRA_HPMCOUNTER16H       : csra_t := x"C90"; --      RW   Upper 32 bits of hpmcounter16, RV32 only
  constant CSRA_HPMCOUNTER17H       : csra_t := x"C91"; --      RW   Upper 32 bits of hpmcounter17, RV32 only
  constant CSRA_HPMCOUNTER18H       : csra_t := x"C92"; --      RW   Upper 32 bits of hpmcounter18, RV32 only
  constant CSRA_HPMCOUNTER19H       : csra_t := x"C93"; --      RW   Upper 32 bits of hpmcounter19, RV32 only
  constant CSRA_HPMCOUNTER20H       : csra_t := x"C94"; --      RW   Upper 32 bits of hpmcounter20, RV32 only
  constant CSRA_HPMCOUNTER21H       : csra_t := x"C95"; --      RW   Upper 32 bits of hpmcounter21, RV32 only
  constant CSRA_HPMCOUNTER22H       : csra_t := x"C96"; --      RW   Upper 32 bits of hpmcounter22, RV32 only
  constant CSRA_HPMCOUNTER23H       : csra_t := x"C97"; --      RW   Upper 32 bits of hpmcounter23, RV32 only
  constant CSRA_HPMCOUNTER24H       : csra_t := x"C98"; --      RW   Upper 32 bits of hpmcounter24, RV32 only
  constant CSRA_HPMCOUNTER25H       : csra_t := x"C99"; --      RW   Upper 32 bits of hpmcounter25, RV32 only
  constant CSRA_HPMCOUNTER26H       : csra_t := x"C9A"; --      RW   Upper 32 bits of hpmcounter26, RV32 only
  constant CSRA_HPMCOUNTER27H       : csra_t := x"C9B"; --      RW   Upper 32 bits of hpmcounter27, RV32 only
  constant CSRA_HPMCOUNTER28H       : csra_t := x"C9C"; --      RW   Upper 32 bits of hpmcounter28, RV32 only
  constant CSRA_HPMCOUNTER29H       : csra_t := x"C9D"; --      RW   Upper 32 bits of hpmcounter29, RV32 only
  constant CSRA_HPMCOUNTER30H       : csra_t := x"C9E"; --      RW   Upper 32 bits of hpmcounter30, RV32 only
  constant CSRA_HPMCOUNTER31H       : csra_t := x"C9F"; --      RO   Upper 32 bits of hpmcounter31, RV32 only
  -- privileged - supervisor level
  constant CSRA_SSTATUS             : csra_t := x"100"; --      RW   Supervisor status register
  constant CSRA_SIE                 : csra_t := x"104"; --      RW   Supervisor interrupt-enable register
  constant CSRA_STVEC               : csra_t := x"105"; --      RW   Supervisor trap handler base address
  constant CSRA_SCOUNTEREN          : csra_t := x"106"; --      RW   Supervisor counter enable
  constant CSRA_SENVCFG             : csra_t := x"10A"; --      RW   Supervisor environment configuration register
  constant CSRA_SSCRATCH            : csra_t := x"140"; --      RW   Scratch register for supervisor trap handlers
  constant CSRA_SEPC                : csra_t := x"141"; --      RW   Supervisor exception program counter
  constant CSRA_SCAUSE              : csra_t := x"142"; --      RW   Supervisor trap cause
  constant CSRA_STVAL               : csra_t := x"143"; --      RW   Supervisor bad address or instruction
  constant CSRA_SIP                 : csra_t := x"144"; --      RW   Supervisor interrupt pending
  constant CSRA_SATP                : csra_t := x"180"; --      RW   Supervisor address translation and protection
  constant CSRA_SCONTEXT            : csra_t := x"5A8"; --      RW   Supervisor-mode context register
  -- privileged - hypervisor level
  constant CSRA_HSTATUS             : csra_t := x"600"; --      RW   Hypervisor status register
  constant CSRA_HEDELEG             : csra_t := x"602"; --      RW   Hypervisor exception delegation register
  constant CSRA_HIDELEG             : csra_t := x"603"; --      RW   Hypervisor interrupt delegation register
  constant CSRA_HIE                 : csra_t := x"604"; --      RW   Hypervisor interrupt-enable register
  constant CSRA_HCOUNTEREN          : csra_t := x"606"; --      RW   Hypervisor counter enable
  constant CSRA_HGEIE               : csra_t := x"607"; --      RW   Hypervisor guest external interrupt-enable register
  constant CSRA_HTVAL               : csra_t := x"643"; --      RW   Hypervisor bad guest physical address
  constant CSRA_HIP                 : csra_t := x"644"; --      RW   Hypervisor interrupt pending
  constant CSRA_HVIP                : csra_t := x"645"; --      RW   Hypervisor virtual interrupt pending
  constant CSRA_HTINST              : csra_t := x"64A"; --      RW   Hypervisor trap instruction (transformed)
  constant CSRA_HGEIP               : csra_t := x"E12"; --      RO   Hypervisor guest external interrupt pending
  constant CSRA_HENVCFG             : csra_t := x"60A"; --      RW   Hypervisor environment configuration register
  constant CSRA_HENVCFGH            : csra_t := x"61A"; --      RW   Additional hypervisor env. conf. register, RV32 only
  constant CSRA_HGATP               : csra_t := x"680"; --      RW   Hypervisor guest address translation and protection
  constant CSRA_HCONTEXT            : csra_t := x"6A8"; --      RW   Hypervisor-mode context register
  constant CSRA_HTIMEDELTA          : csra_t := x"605"; --      RW   Delta for VS/VU-mode timer
  constant CSRA_HTIMEDELTAH         : csra_t := x"615"; --      RW   Upper 32 bits of htimedelta, HSXLEN=32 only
  constant CSRA_VSSTATUS            : csra_t := x"200"; --      RW   Virtual supervisor status register
  constant CSRA_VSIE                : csra_t := x"204"; --      RW   Virtual supervisor interrupt-enable register
  constant CSRA_VSTVEC              : csra_t := x"205"; --      RW   Virtual supervisor trap handler base address
  constant CSRA_VSSCRATCH           : csra_t := x"240"; --      RW   Virtual supervisor scratch register
  constant CSRA_VSEPC               : csra_t := x"241"; --      RW   Virtual supervisor exception program counter
  constant CSRA_VSCAUSE             : csra_t := x"242"; --      RW   Virtual supervisor trap cause
  constant CSRA_VSTVAL              : csra_t := x"243"; --      RW   Virtual supervisor bad address or instruction
  constant CSRA_VSIP                : csra_t := x"244"; --      RW   Virtual supervisor interrupt pending
  constant CSRA_VSATP               : csra_t := x"280"; --      RW   Virtual supervisor address translation and protection
  -- privileged - machine level
  constant CSRA_MVENDORID           : csra_t := x"F11"; -- all  RO   Vendor ID
  constant CSRA_MARCHID             : csra_t := x"F12"; -- all  RO   Architecture ID
  constant CSRA_MIMPID              : csra_t := x"F13"; -- all  RO   Implementation ID
  constant CSRA_MHARTID             : csra_t := x"F14"; -- all  RO   Hardware thread ID
  constant CSRA_MCONFIGPTR          : csra_t := x"F15"; -- all  RO   Pointer to configuration data structure
  constant CSRA_MSTATUS             : csra_t := x"300"; -- all  RW   Machine status register
  constant CSRA_MISA                : csra_t := x"301"; -- all  RW   ISA and extensions
  constant CSRA_MEDELEG             : csra_t := x"302"; -- <=S  RW   Machine exception delegation register
  constant CSRA_MIDELEG             : csra_t := x"303"; -- <=S  RW   Machine interrupt delegation register
  constant CSRA_MIE                 : csra_t := x"304"; -- all  RW   Machine interrupt-enable register
  constant CSRA_MTVEC               : csra_t := x"305"; -- all  RW   Machine trap-handler base address
  constant CSRA_MCOUNTEREN          : csra_t := x"306"; -- U    RW   Machine counter enable
  constant CSRA_MSTATUSH            : csra_t := x"310"; -- all  RW   Additional machine status register, RV32 only
  constant CSRA_MSCRATCH            : csra_t := x"340"; -- all  RW   Scratch register for machine trap handlers
  constant CSRA_MEPC                : csra_t := x"341"; -- all  RW   Machine exception program counter
  constant CSRA_MCAUSE              : csra_t := x"342"; -- all  RW   Machine trap cause
  constant CSRA_MTVAL               : csra_t := x"343"; -- all  RW   Machine bad address or instruction
  constant CSRA_MIP                 : csra_t := x"344"; -- all  RW   Machine interrupt pending
  constant CSRA_MTINST              : csra_t := x"34A"; -- all  RW   Machine trap instruction (transformed)
  constant CSRA_MTVAL2              : csra_t := x"34B"; -- all  RW   Machine bad guest physical address
  constant CSRA_MENVCFG             : csra_t := x"30A"; -- U    RW   Machine environment configuration register
  constant CSRA_MENVCFGH            : csra_t := x"31A"; -- U    RW   Additional machine env. conf. register, RV32 only
  constant CSRA_MSECCFG             : csra_t := x"747"; --      RW   Machine security configuration register
  constant CSRA_MSECCFGH            : csra_t := x"757"; --      RW   Additional machine security conf. register, RV32 only
  constant CSRA_PMPCFG0             : csra_t := x"3A0"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG1             : csra_t := x"3A1"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG2             : csra_t := x"3A2"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG3             : csra_t := x"3A3"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG4             : csra_t := x"3A4"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG5             : csra_t := x"3A5"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG6             : csra_t := x"3A6"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG7             : csra_t := x"3A7"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG8             : csra_t := x"3A8"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG9             : csra_t := x"3A9"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG10            : csra_t := x"3AA"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG11            : csra_t := x"3AB"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG12            : csra_t := x"3AC"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG13            : csra_t := x"3AD"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPCFG14            : csra_t := x"3AE"; --      RW   Physical memory protection configuration
  constant CSRA_PMPCFG15            : csra_t := x"3AF"; --      RW   Physical memory protection configuration, RV32 only
  constant CSRA_PMPADDR0            : csra_t := x"3B0"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR1            : csra_t := x"3B1"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR2            : csra_t := x"3B2"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR3            : csra_t := x"3B3"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR4            : csra_t := x"3B4"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR5            : csra_t := x"3B5"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR6            : csra_t := x"3B6"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR7            : csra_t := x"3B7"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR8            : csra_t := x"3B8"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR9            : csra_t := x"3B9"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR10           : csra_t := x"3BA"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR11           : csra_t := x"3BB"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR12           : csra_t := x"3BC"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR13           : csra_t := x"3BD"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR14           : csra_t := x"3BE"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR15           : csra_t := x"3BF"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR16           : csra_t := x"3C0"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR17           : csra_t := x"3C1"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR18           : csra_t := x"3C2"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR19           : csra_t := x"3C3"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR20           : csra_t := x"3C4"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR21           : csra_t := x"3C5"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR22           : csra_t := x"3C6"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR23           : csra_t := x"3C7"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR24           : csra_t := x"3C8"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR25           : csra_t := x"3C9"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR26           : csra_t := x"3CA"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR27           : csra_t := x"3CB"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR28           : csra_t := x"3CC"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR29           : csra_t := x"3CD"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR30           : csra_t := x"3CE"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR31           : csra_t := x"3CF"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR32           : csra_t := x"3D0"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR33           : csra_t := x"3D1"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR34           : csra_t := x"3D2"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR35           : csra_t := x"3D3"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR36           : csra_t := x"3D4"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR37           : csra_t := x"3D5"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR38           : csra_t := x"3D6"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR39           : csra_t := x"3D7"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR40           : csra_t := x"3D8"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR41           : csra_t := x"3D9"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR42           : csra_t := x"3DA"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR43           : csra_t := x"3DB"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR44           : csra_t := x"3DC"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR45           : csra_t := x"3DD"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR46           : csra_t := x"3DE"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR47           : csra_t := x"3DF"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR48           : csra_t := x"3E0"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR49           : csra_t := x"3E1"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR50           : csra_t := x"3E2"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR51           : csra_t := x"3E3"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR52           : csra_t := x"3E4"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR53           : csra_t := x"3E5"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR54           : csra_t := x"3E6"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR55           : csra_t := x"3E7"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR56           : csra_t := x"3E8"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR57           : csra_t := x"3E9"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR58           : csra_t := x"3EA"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR59           : csra_t := x"3EB"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR60           : csra_t := x"3EC"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR61           : csra_t := x"3ED"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR62           : csra_t := x"3EE"; --      RW   Physical memory protection address register
  constant CSRA_PMPADDR63           : csra_t := x"3EF"; --      RW   Physical memory protection address register
  constant CSRA_MCYCLE              : csra_t := x"B00"; -- all  RW   Machine cycle counter
  constant CSRA_MINSTRET            : csra_t := x"B02"; -- all  RW   Machine instructions-retired counter
  constant CSRA_MHPMCOUNTER3        : csra_t := x"B03"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER4        : csra_t := x"B04"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER5        : csra_t := x"B05"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER6        : csra_t := x"B06"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER7        : csra_t := x"B07"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER8        : csra_t := x"B08"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER9        : csra_t := x"B09"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER10       : csra_t := x"B0A"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER11       : csra_t := x"B0B"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER12       : csra_t := x"B0C"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER13       : csra_t := x"B0D"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER14       : csra_t := x"B0E"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER15       : csra_t := x"B0F"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER16       : csra_t := x"B10"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER17       : csra_t := x"B11"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER18       : csra_t := x"B12"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER19       : csra_t := x"B13"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER20       : csra_t := x"B14"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER21       : csra_t := x"B15"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER22       : csra_t := x"B16"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER23       : csra_t := x"B17"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER24       : csra_t := x"B18"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER25       : csra_t := x"B19"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER26       : csra_t := x"B1A"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER27       : csra_t := x"B1B"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER28       : csra_t := x"B1C"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER29       : csra_t := x"B1D"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER30       : csra_t := x"B1E"; --      RW   Machine performance-monitoring counter
  constant CSRA_MHPMCOUNTER31       : csra_t := x"B1F"; --      RW   Machine performance-monitoring counter
  constant CSRA_MCYCLEH             : csra_t := x"B80"; -- all  RW   Upper 32 bits of mcycle, RV32 only
  constant CSRA_MINSTRETH           : csra_t := x"B82"; -- all  RW   Upper 32 bits of minstret, RV32 only
  constant CSRA_MHPMCOUNTER3H       : csra_t := x"B83"; --      RW   Upper 32 bits of mhpmcounter3, RV32 only
  constant CSRA_MHPMCOUNTER4H       : csra_t := x"B84"; --      RW   Upper 32 bits of mhpmcounter4, RV32 only
  constant CSRA_MHPMCOUNTER5H       : csra_t := x"B85"; --      RW   Upper 32 bits of mhpmcounter5, RV32 only
  constant CSRA_MHPMCOUNTER6H       : csra_t := x"B86"; --      RW   Upper 32 bits of mhpmcounter6, RV32 only
  constant CSRA_MHPMCOUNTER7H       : csra_t := x"B87"; --      RW   Upper 32 bits of mhpmcounter7, RV32 only
  constant CSRA_MHPMCOUNTER8H       : csra_t := x"B88"; --      RW   Upper 32 bits of mhpmcounter8, RV32 only
  constant CSRA_MHPMCOUNTER9H       : csra_t := x"B89"; --      RW   Upper 32 bits of mhpmcounter9, RV32 only
  constant CSRA_MHPMCOUNTER10H      : csra_t := x"B8A"; --      RW   Upper 32 bits of mhpmcounter10, RV32 only
  constant CSRA_MHPMCOUNTER11H      : csra_t := x"B8B"; --      RW   Upper 32 bits of mhpmcounter11, RV32 only
  constant CSRA_MHPMCOUNTER12H      : csra_t := x"B8C"; --      RW   Upper 32 bits of mhpmcounter12, RV32 only
  constant CSRA_MHPMCOUNTER13H      : csra_t := x"B8D"; --      RW   Upper 32 bits of mhpmcounter13, RV32 only
  constant CSRA_MHPMCOUNTER14H      : csra_t := x"B8E"; --      RW   Upper 32 bits of mhpmcounter14, RV32 only
  constant CSRA_MHPMCOUNTER15H      : csra_t := x"B8F"; --      RW   Upper 32 bits of mhpmcounter15, RV32 only
  constant CSRA_MHPMCOUNTER16H      : csra_t := x"B90"; --      RW   Upper 32 bits of mhpmcounter16, RV32 only
  constant CSRA_MHPMCOUNTER17H      : csra_t := x"B91"; --      RW   Upper 32 bits of mhpmcounter17, RV32 only
  constant CSRA_MHPMCOUNTER18H      : csra_t := x"B92"; --      RW   Upper 32 bits of mhpmcounter18, RV32 only
  constant CSRA_MHPMCOUNTER19H      : csra_t := x"B93"; --      RW   Upper 32 bits of mhpmcounter19, RV32 only
  constant CSRA_MHPMCOUNTER20H      : csra_t := x"B94"; --      RW   Upper 32 bits of mhpmcounter20, RV32 only
  constant CSRA_MHPMCOUNTER21H      : csra_t := x"B95"; --      RW   Upper 32 bits of mhpmcounter21, RV32 only
  constant CSRA_MHPMCOUNTER22H      : csra_t := x"B96"; --      RW   Upper 32 bits of mhpmcounter22, RV32 only
  constant CSRA_MHPMCOUNTER23H      : csra_t := x"B97"; --      RW   Upper 32 bits of mhpmcounter23, RV32 only
  constant CSRA_MHPMCOUNTER24H      : csra_t := x"B98"; --      RW   Upper 32 bits of mhpmcounter24, RV32 only
  constant CSRA_MHPMCOUNTER25H      : csra_t := x"B99"; --      RW   Upper 32 bits of mhpmcounter25, RV32 only
  constant CSRA_MHPMCOUNTER26H      : csra_t := x"B9A"; --      RW   Upper 32 bits of mhpmcounter26, RV32 only
  constant CSRA_MHPMCOUNTER27H      : csra_t := x"B9B"; --      RW   Upper 32 bits of mhpmcounter27, RV32 only
  constant CSRA_MHPMCOUNTER28H      : csra_t := x"B9C"; --      RW   Upper 32 bits of mhpmcounter28, RV32 only
  constant CSRA_MHPMCOUNTER29H      : csra_t := x"B9D"; --      RW   Upper 32 bits of mhpmcounter29, RV32 only
  constant CSRA_MHPMCOUNTER30H      : csra_t := x"B9E"; --      RW   Upper 32 bits of mhpmcounter30, RV32 only
  constant CSRA_MHPMCOUNTER31H      : csra_t := x"B9F"; --      RW   Upper 32 bits of mhpmcounter31, RV32 only
  constant CSRA_MCOUNTINHIBIT       : csra_t := x"320"; --      RW   Machine counter-inhibit register
  constant CSRA_MHPMEVENT3          : csra_t := x"323"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT4          : csra_t := x"324"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT5          : csra_t := x"325"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT6          : csra_t := x"326"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT7          : csra_t := x"327"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT8          : csra_t := x"328"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT9          : csra_t := x"329"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT10         : csra_t := x"32A"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT11         : csra_t := x"32B"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT12         : csra_t := x"32C"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT13         : csra_t := x"32D"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT14         : csra_t := x"32E"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT15         : csra_t := x"32F"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT16         : csra_t := x"330"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT17         : csra_t := x"331"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT18         : csra_t := x"332"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT19         : csra_t := x"333"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT20         : csra_t := x"334"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT21         : csra_t := x"335"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT22         : csra_t := x"336"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT23         : csra_t := x"337"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT24         : csra_t := x"338"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT25         : csra_t := x"339"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT26         : csra_t := x"33A"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT27         : csra_t := x"33B"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT28         : csra_t := x"33C"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT29         : csra_t := x"33D"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT30         : csra_t := x"33E"; --      RW   Machine performance-monitoring event selector
  constant CSRA_MHPMEVENT31         : csra_t := x"33F"; --      RW   Machine performance-monitoring event selector
  constant CSRA_TSELECT             : csra_t := x"7A0"; --      RW   Debug/Trace trigger register select
  constant CSRA_TDATA1              : csra_t := x"7A1"; --      RW   First Debug/Trace trigger data register
  constant CSRA_TDATA2              : csra_t := x"7A2"; --      RW   Second Debug/Trace trigger data register
  constant CSRA_TDATA3              : csra_t := x"7A3"; --      RW   Third Debug/Trace trigger data register
  constant CSRA_MCONTEXT            : csra_t := x"7A8"; --      RW   Machine-mode context register
  constant CSRA_DCSR                : csra_t := x"7B0"; --      RW   Debug control and status register
  constant CSRA_DPC                 : csra_t := x"7B1"; --      RW   Debug PC
  constant CSRA_DSCRATCH0           : csra_t := x"7B2"; --      RW   Debug scratch register 0
  constant CSRA_DSCRATCH1           : csra_t := x"7B3"; --      RW   Debug scratch register 1

  --------------------------------------------------------------------------------
  -- CSR register bits

  constant CSRB_MSTATUS_WPRI0   : integer := 0;
  constant CSRB_MSTATUS_SIE     : integer := 1;
  constant CSRB_MSTATUS_WPRI2   : integer := 2;
  constant CSRB_MSTATUS_MIE     : integer := 3;
  constant CSRB_MSTATUS_WPRI4   : integer := 4;
  constant CSRB_MSTATUS_SPIE    : integer := 5;
  constant CSRB_MSTATUS_UBE     : integer := 6;
  constant CSRB_MSTATUS_MPIE    : integer := 7;
  constant CSRB_MSTATUS_SPP     : integer := 8;
  constant CSRB_MSTATUS_VS0     : integer := 9;
  constant CSRB_MSTATUS_VS1     : integer := 10;
  constant CSRB_MSTATUS_MPP0    : integer := 11;
  constant CSRB_MSTATUS_MPP1    : integer := 12;
  constant CSRB_MSTATUS_FS0     : integer := 13;
  constant CSRB_MSTATUS_FS1     : integer := 14;
  constant CSRB_MSTATUS_XS0     : integer := 15;
  constant CSRB_MSTATUS_XS1     : integer := 16;
  constant CSRB_MSTATUS_MPRV    : integer := 17;
  constant CSRB_MSTATUS_SUM     : integer := 18;
  constant CSRB_MSTATUS_MXR     : integer := 19;
  constant CSRB_MSTATUS_TVM     : integer := 20;
  constant CSRB_MSTATUS_TW      : integer := 21;
  constant CSRB_MSTATUS_TSR     : integer := 22;
  constant CSRB_MSTATUS_WPRI23  : integer := 23;
  constant CSRB_MSTATUS_WPRI30  : integer := 30;
  constant CSRB_MSTATUS_SD31    : integer := 31;

  constant CSRB_MSTATUS_UXL0    : integer := 32;
  constant CSRB_MSTATUS_UXL1    : integer := 33;
  constant CSRB_MSTATUS_SXL0    : integer := 34;
  constant CSRB_MSTATUS_SXL1    : integer := 35;
  constant CSRB_MSTATUS_SBE     : integer := 36;
  constant CSRB_MSTATUS_MBE     : integer := 37;
  constant CSRB_MSTATUS_WPRI38  : integer := 38;
  constant CSRB_MSTATUS_WPRI62  : integer := 20;
  constant CSRB_MSTATUS_SD63    : integer := 63;

  constant CSRB_MIE_SSIE        : integer := 1;
  constant CSRB_MIE_MSIE        : integer := 3;
  constant CSRB_MIE_STIE        : integer := 5;
  constant CSRB_MIE_MTIE        : integer := 7;
  constant CSRB_MIE_SEIE        : integer := 9;
  constant CSRB_MIE_MEIE        : integer := 11;

  constant CSRB_MTVEC_MODE0     : integer := 0;
  constant CSRB_MTVEC_MODE1     : integer := 1;

  constant CSRB_MSTATUSH_WPRI0  : integer := 0;
  constant CSRB_MSTATUSH_WPRI3  : integer := 3;
  constant CSRB_MSTATUSH_SBE    : integer := 4;
  constant CSRB_MSTATUSH_MBE    : integer := 5;
  constant CSRB_MSTATUSH_WPRI6  : integer := 6;
  constant CSRB_MSTATUSH_WPRI31 : integer := 31;

  constant CSRB_MIP_SSIP        : integer := 1;
  constant CSRB_MIP_MSIP        : integer := 3;
  constant CSRB_MIP_STIP        : integer := 5;
  constant CSRB_MIP_MTIP        : integer := 7;
  constant CSRB_MIP_SEIP        : integer := 9;
  constant CSRB_MIP_MEIP        : integer := 11;

  --------------------------------------------------------------------------------

  function bool2sl( cond : boolean ) return std_ulogic;
  function pattern_match(v,m : std_ulogic_vector) return boolean;

end package common_pkg;

package body common_pkg is

  function bool2sl( cond : boolean ) return std_ulogic is
  begin
    if cond then return '1'; else return '0'; end if;
  end function bool2sl;

  function pattern_match(v,m : std_ulogic_vector) return boolean is
    variable r : boolean;
  begin
    if v'low /= m'low or v'high /= m'high then return false; end if;
    r := true;
    for i in v'low to v'high loop
      if m(i) /= '-' and m(i) /= v(i) then r := false; exit; end if;
    end loop;
    return r;
  end function pattern_match;

end package body common_pkg;
