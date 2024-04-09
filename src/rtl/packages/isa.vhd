--------------------------------------------------------------------------------
-- isa.vhd                                                                    --
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

use work.common_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;

package isa_pkg is

  type opcode_t is (
  --  | 000    | 001      | 010      | 011      | 100    | 101    | 110       | 111    | <-- inst[4:2]
        LOAD,    LOAD_FP,   CUSTOM_0,  MISC_MEM,  OP_IMM,  AUIPC,   OP_IMM_32, LEN_48A,  -- 00 } inst[6:5]
        STORE,   STORE_FP,  CUSTOM_1,  AMO,       OP,      LUI,     OP_32,     LEN_64,   -- 01 }
        MADD,    MSUB,      NMSUB,     NMADD,     OP_FP,   RSVD_0,  CUSTOM_2,  LEN_48B,  -- 10 }
        BRANCH,  JALR,      RSVD_1,    JAL,       SYSTEM,  RSVD_2,  CUSTOM_3,  LEN_80    -- 11 }
  );

  -- uncompressed instruction mnemonics
  type mnemonic_t is (
    -- unknown
    U_UNKNOWN,
    -- RV32I Base
    U_ADD, U_SUB, U_SLL, U_SLT, U_SLTU, U_XOR, U_SRL, U_SRA, U_OR, U_AND,
    U_JALR,
    U_LB, U_LH, U_LW, U_LBU, U_LHU,
    U_ADDI, U_SLLI, U_SLTI, U_SLTIU, U_XORI, U_SRLI, U_SRAI, U_ORI, U_ANDI,
    U_FENCE,
    U_ECALL, U_EBREAK,
    U_SB, U_SH, U_SW,
    U_BEQ, U_BNE, U_BLT, U_BGE, U_BLTU, U_BGEU,
    U_LUI, U_AUIPC,
    U_JAL,
    -- RV64I Base
    U_LWU, U_LD,
    U_SD,
    U_ADDIW, U_SLLIW, U_SRLIW, U_SRAIW,
    U_ADDW, U_SUBW, U_SLLW, U_SRLW, U_SRAW,
    -- Zicsr
    U_CSRRW, U_CSRRS, U_CSRRC, U_CSRRWI, U_CSRRSI, U_CSRRCI,
    -- RV32I Privileged
    U_MRET, U_WFI
  );

  type isa_t is record
    XLEN     : positive;
    I        : boolean;
    E        : boolean;
    M        : boolean;
    A        : boolean;
    F        : boolean;
    D        : boolean;
    C        : boolean;
    Zicsr    : boolean;
    Zifencei : boolean;
  end record isa_t;

  type inst_def_t is record
    mnemonic : mnemonic_t;
    pattern  : inst_t;
    rs1      : std_ulogic;
    rs2      : std_ulogic;
    rd       : std_ulogic;
  end record inst_def_t;

  type inst_set_t is array(natural range <>) of inst_def_t;

  constant F3_0       : f3_t  := "000";
  constant F3_ADDSUB  : f3_t  := "000"; -- for ADD and SUB
  constant F3_SLL     : f3_t  := "001";
  constant F3_SLT     : f3_t  := "010";
  constant F3_SLTU    : f3_t  := "011";
  constant F3_XOR     : f3_t  := "100";
  constant F3_SR      : f3_t  := "101"; -- for SRL and SRA
  constant F3_OR      : f3_t  := "110";
  constant F3_AND     : f3_t  := "111";
  constant F3_SZ_B    : f3_t  := '0' & SZ_B;
  constant F3_SZ_H    : f3_t  := '0' & SZ_H;
  constant F3_SZ_W    : f3_t  := '0' & SZ_W;
  constant F3_SZ_D    : f3_t  := '0' & SZ_D;
  constant F3_SZ_BU   : f3_t  := '1' & SZ_B;
  constant F3_SZ_HU   : f3_t  := '1' & SZ_H;
  constant F3_SZ_WU   : f3_t  := '1' & SZ_W;
  constant F3_ADDI    : f3_t  := "000";
  constant F3_SLLI    : f3_t  := "001";
  constant F3_SLTI    : f3_t  := "010";
  constant F3_SLTIU   : f3_t  := "011";
  constant F3_XORI    : f3_t  := "100";
  constant F3_SRI     : f3_t  := "101";
  constant F3_ORI     : f3_t  := "110";
  constant F3_ANDI    : f3_t  := "111";
  constant F3_BEQ     : f3_t  := "000";
  constant F3_BNE     : f3_t  := "001";
  constant F3_BLT     : f3_t  := "100";
  constant F3_BGE     : f3_t  := "101";
  constant F3_BLTU    : f3_t  := "110";
  constant F3_BGEU    : f3_t  := "111";
  constant F3_CSRRW   : f3_t  := "001";
  constant F3_CSRRS   : f3_t  := "010";
  constant F3_CSRRC   : f3_t  := "011";
  constant F3_CSRRWI  : f3_t  := "101";
  constant F3_CSRRSI  : f3_t  := "110";
  constant F3_CSRRCI  : f3_t  := "111";
  constant F7_ADD     : f7_t  := "0000000";
  constant F7_SUB     : f7_t  := "0100000";
  constant F7_SLL     : f7_t  := "0000000";
  constant F7_SLL64   : f7_t  := "000000-";
  constant F7_SRL     : f7_t  := "0000000";
  constant F7_SRL64   : f7_t  := "000000-";
  constant F7_SRA     : f7_t  := "0100000";
  constant F7_SRA64   : f7_t  := "010000-";
  constant F12_ECALL  : f12_t := "000000000000";
  constant F12_EBREAK : f12_t := "000000000001";
  constant F12_MRET   : f12_t := "001100000010";
  constant F12_WFI    : f12_t := "000100000101";

  function isa_decode(s : string) return isa_t;
  function inst_set(isa : isa_t) return inst_set_t;
  function uncompress(u : inst_t; isa : isa_t) return inst_t;

end package isa_pkg;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

package body isa_pkg is

  --------------------------------------------------------------------------------

  function isa_decode(s : string) return isa_t is
    variable r : isa_t;
    variable n : string(s'range) := s;
    constant l : integer := s'length;
  begin
    r := (XLEN => 32, I => true, others => false);
    assert l >= 5
      report "invalid/incomplete ISA name : " & s severity failure;
    assert n(1 to 2) = "RV"
      report "invalid ISA name prefix : " & s severity failure;
    assert n(3 to 4) = "32" or n(3 to 4) = "64"
      report "invalid ISA name XLEN : " & s severity failure;
    assert n(5) = 'I'
      report "invalid base ISA : " & s severity failure;
    if n(3 to 4) = "64" then
      r.XLEN := 64;
    end if;
    if l < 6 then
      return r;
    end if;
    n := n(6 to l) & "     ";
    loop
         if n(1)      = ' '        then exit;
      elsif n(1)      = '_'        then                     n := n(2 to l) & " ";
      elsif n(1)      = 'M'        then r.M        := true; n := n(2 to l) & " ";
      elsif n(1)      = 'A'        then r.A        := true; n := n(2 to l) & " ";
      elsif n(1)      = 'F'        then r.F        := true; n := n(2 to l) & " ";
      elsif n(1)      = 'D'        then r.D        := true; n := n(2 to l) & " ";
      elsif n(1)      = 'C'        then r.C        := true; n := n(2 to l) & " ";
      elsif n(1 to 5) = "Zicsr"    then r.Zicsr    := true; n := n(6 to l) & "     ";
      elsif n(1 to 8) = "Zifencei" then r.Zifencei := true; n := n(9 to l) & "        ";
      elsif n(1)      = 'G'        then
        r.I        := true;
        r.M        := true;
        r.A        := true;
        r.F        := true;
        r.D        := true;
        r.Zicsr    := true;
        r.Zifencei := true;
        n := n(2 to l) & " ";
      else
        assert false
          report "unsupported ISA extension: " & s severity failure;
      end if;
    end loop;
    if r.D then
      r.F := true;
    end if;
    if r.F then
      r.Zicsr := true;
    end if;
    return r;
  end function isa_decode;

  --------------------------------------------------------------------------------
  -- uncompressed instructions

  -- convert opcode_t to 5 bit opcode vector
  function opvec(op : opcode_t) return opvec_t is
  begin
    return std_ulogic_vector(to_unsigned(opcode_t'pos(op),5));
  end function opvec;

  -- R format with f3
  function R_fmt(op : opcode_t; f3 : f3_t) return inst_t is
  begin
    return "-------" & "-----" & "-----" & f3 & "-----" & opvec(op) & "11";
  end function R_fmt;

  -- R format with f3,f7
  function R_fmt(op : opcode_t; f3 : f3_t; f7 : f7_t) return inst_t is
  begin
    return f7 & "-----" & "-----" & f3 & "-----" & opvec(op) & "11";
  end function R_fmt;

  -- I format with f3
  function I_fmt(op : opcode_t; f3 : f3_t) return inst_t is
  begin
    return "------------" & "-----" & f3 & "-----" & opvec(op) & "11";
  end function I_fmt;

  -- I format with f3,f7
  function I_fmt(op : opcode_t; f3 : f3_t; f7 : f7_t) return inst_t is
  begin
    return f7 & "-----" & "-----" & f3 & "-----" & opvec(op) & "11";
  end function I_fmt;

  -- I format with f3,imm12
  function IS_fmt(op : opcode_t; f3 : f3_t; f12 : f12_t) return inst_t is
  begin
    return f12 & "-----" & f3 & "-----" & opvec(op) & "11";
  end function IS_fmt;

  -- S format with f3
  function S_fmt(op : opcode_t; f3 : f3_t) return inst_t is
  begin
    return "-------" & "-----" & "-----" & f3 & "-----" & opvec(op) & "11";
  end function S_fmt;

  -- B format with f3
  function B_fmt(op : opcode_t; f3 : f3_t) return inst_t is
  begin
    return "-----------------" & f3 & "-----" & opvec(op) & "11";
  end function B_fmt;

  -- U format
  function U_fmt(op : opcode_t) return inst_t is
  begin
    return "--------------------" & "-----" & opvec(op) & "11";
  end function U_fmt;

  constant inst_set_RV_Base : inst_set_t := (
    ( U_ADD    , R_fmt  ( OP       , F3_ADDSUB , F7_ADD     ) , '1' , '1' , '1' ),
    ( U_SUB    , R_fmt  ( OP       , F3_ADDSUB , F7_SUB     ) , '1' , '1' , '1' ),
    ( U_SLL    , R_fmt  ( OP       , F3_SLL                 ) , '1' , '1' , '1' ),
    ( U_SLT    , R_fmt  ( OP       , F3_SLT                 ) , '1' , '1' , '1' ),
    ( U_SLTU   , R_fmt  ( OP       , F3_SLTU                ) , '1' , '1' , '1' ),
    ( U_XOR    , R_fmt  ( OP       , F3_XOR                 ) , '1' , '1' , '1' ),
    ( U_SRL    , R_fmt  ( OP       , F3_SR     , F7_SRL     ) , '1' , '1' , '1' ),
    ( U_SRA    , R_fmt  ( OP       , F3_SR     , F7_SRA     ) , '1' , '1' , '1' ),
    ( U_OR     , R_fmt  ( OP       , F3_OR                  ) , '1' , '1' , '1' ),
    ( U_AND    , R_fmt  ( OP       , F3_AND                 ) , '1' , '1' , '1' ),
    ( U_ADDI   , I_fmt  ( OP_IMM   , F3_ADDI                ) , '1' , '0' , '1' ),
    ( U_SLTI   , I_fmt  ( OP_IMM   , F3_SLTI                ) , '1' , '0' , '1' ),
    ( U_SLTIU  , I_fmt  ( OP_IMM   , F3_SLTIU               ) , '1' , '0' , '1' ),
    ( U_XORI   , I_fmt  ( OP_IMM   , F3_XORI                ) , '1' , '0' , '1' ),
    ( U_ORI    , I_fmt  ( OP_IMM   , F3_ORI                 ) , '1' , '0' , '1' ),
    ( U_ANDI   , I_fmt  ( OP_IMM   , F3_ANDI                ) , '1' , '0' , '1' ),
    ( U_LB     , I_fmt  ( LOAD     , F3_SZ_B                ) , '1' , '0' , '1' ),
    ( U_LH     , I_fmt  ( LOAD     , F3_SZ_H                ) , '1' , '0' , '1' ),
    ( U_LW     , I_fmt  ( LOAD     , F3_SZ_W                ) , '1' , '0' , '1' ),
    ( U_LBU    , I_fmt  ( LOAD     , F3_SZ_BU               ) , '1' , '0' , '1' ),
    ( U_LHU    , I_fmt  ( LOAD     , F3_SZ_HU               ) , '1' , '0' , '1' ),
    ( U_SB     , S_fmt  ( STORE    , F3_SZ_B                ) , '1' , '1' , '0' ),
    ( U_SH     , S_fmt  ( STORE    , F3_SZ_H                ) , '1' , '1' , '0' ),
    ( U_SW     , S_fmt  ( STORE    , F3_SZ_W                ) , '1' , '1' , '0' ),
    ( U_JAL    , U_fmt  ( JAL                               ) , '1' , '0' , '1' ),
    ( U_JALR   , I_fmt  ( JALR     , F3_0                   ) , '1' , '0' , '1' ),
    ( U_BEQ    , B_fmt  ( BRANCH   , F3_BEQ                 ) , '1' , '1' , '0' ),
    ( U_BNE    , B_fmt  ( BRANCH   , F3_BNE                 ) , '1' , '1' , '0' ),
    ( U_BLT    , B_fmt  ( BRANCH   , F3_BLT                 ) , '1' , '1' , '0' ),
    ( U_BGE    , B_fmt  ( BRANCH   , F3_BGE                 ) , '1' , '1' , '0' ),
    ( U_BLTU   , B_fmt  ( BRANCH   , F3_BLTU                ) , '1' , '1' , '0' ),
    ( U_BGEU   , B_fmt  ( BRANCH   , F3_BGEU                ) , '1' , '1' , '0' ),
    ( U_LUI    , U_fmt  ( LUI                               ) , '1' , '0' , '1' ),
    ( U_AUIPC  , U_fmt  ( AUIPC                             ) , '1' , '0' , '1' ),
    ( U_FENCE  , I_fmt  ( MISC_MEM , F3_0                   ) , '0' , '0' , '0' ),
    ( U_ECALL  , IS_fmt ( SYSTEM   , F3_0      , F12_ECALL  ) , '0' , '0' , '0' ),
    ( U_EBREAK , IS_fmt ( SYSTEM   , F3_0      , F12_EBREAK ) , '0' , '0' , '0' )
  );

  constant inst_set_RV32_Base : inst_set_t := (
    ( U_SLLI   , I_fmt  ( OP_IMM   , F3_SLLI   , F7_SLL     ) , '1' , '0' , '1' ),
    ( U_SRLI   , I_fmt  ( OP_IMM   , F3_SRI    , F7_SRL     ) , '1' , '0' , '1' ),
    ( U_SRAI   , I_fmt  ( OP_IMM   , F3_SRI    , F7_SRA     ) , '1' , '0' , '1' )
  );

  constant inst_set_RV64_Base : inst_set_t := (
    ( U_SLLI   , I_fmt  ( OP_IMM    , F3_SLLI   , F7_SLL64   ) , '1' , '0' , '1' ),
    ( U_SRLI   , I_fmt  ( OP_IMM    , F3_SRI    , F7_SRL64   ) , '1' , '0' , '1' ),
    ( U_SRAI   , I_fmt  ( OP_IMM    , F3_SRI    , F7_SRA64   ) , '1' , '0' , '1' ),
    ( U_ADDW   , R_fmt  ( OP_32     , F3_ADDSUB , F7_ADD     ) , '1' , '1' , '1' ),
    ( U_SUBW   , R_fmt  ( OP_32     , F3_ADDSUB , F7_SUB     ) , '1' , '1' , '1' ),
    ( U_SLLW   , R_fmt  ( OP_32     , F3_SLL                 ) , '1' , '1' , '1' ),
    ( U_SRLW   , R_fmt  ( OP_32     , F3_SR     , F7_SRL     ) , '1' , '1' , '1' ),
    ( U_SRAW   , R_fmt  ( OP_32     , F3_SR     , F7_SRA     ) , '1' , '1' , '1' ),
    ( U_ADDIW  , I_fmt  ( OP_IMM_32 , F3_ADDI                ) , '1' , '0' , '1' ),
    ( U_SLLIW  , I_fmt  ( OP_IMM_32 , F3_SLLI   , F7_SLL     ) , '1' , '0' , '1' ),
    ( U_SRLIW  , I_fmt  ( OP_IMM_32 , F3_SRI    , F7_SRL     ) , '1' , '0' , '1' ),
    ( U_SRAIW  , I_fmt  ( OP_IMM_32 , F3_SRI    , F7_SRA     ) , '1' , '0' , '1' ),
    ( U_LD     , I_fmt  ( LOAD      , F3_SZ_D                ) , '1' , '0' , '1' ),
    ( U_LWU    , I_fmt  ( LOAD      , F3_SZ_WU               ) , '1' , '0' , '1' ),
    ( U_SD     , S_fmt  ( STORE     , F3_SZ_D                ) , '1' , '1' , '0' )
  );

  constant inst_set_RV_Zicsr : inst_set_t := (
    ( U_CSRRW  , I_fmt  ( SYSTEM  , F3_CSRRW                 ) , '1' , '0' , '1' ),
    ( U_CSRRS  , I_fmt  ( SYSTEM  , F3_CSRRS                 ) , '1' , '0' , '1' ),
    ( U_CSRRC  , I_fmt  ( SYSTEM  , F3_CSRRC                 ) , '1' , '0' , '1' ),
    ( U_CSRRWI , I_fmt  ( SYSTEM  , F3_CSRRWI                ) , '1' , '0' , '1' ),
    ( U_CSRRSI , I_fmt  ( SYSTEM  , F3_CSRRSI                ) , '1' , '0' , '1' ),
    ( U_CSRRCI , I_fmt  ( SYSTEM  , F3_CSRRCI                ) , '1' , '0' , '1' )
  );

  -- TODO add SRET
  constant inst_set_RV_Priv : inst_set_t := (
    ( U_MRET   , IS_fmt ( SYSTEM  , F3_0        , F12_MRET   ) , '0' , '0' , '0' ),
    ( U_WFI    , IS_fmt ( SYSTEM  , F3_0        , F12_WFI    ) , '0' , '0' , '0' )
  );

  function inst_set(isa : isa_t) return inst_set_t is
  begin
    assert isa.I and not isa.E and (isa.XLEN = 32 or isa.XLEN = 64)
      report "unsupported base ISA" severity failure;
    assert not isa.M
      report "M extension not yet supported" severity failure;
    assert not isa.A
      report "A extension not yet supported" severity failure;
    assert isa.F nor isa.D
      report "floating point extensions not yet supported" severity failure;
    assert not isa.Zifencei
      report "Zifencei extension not yet supported" severity failure;
    if isa.XLEN = 32 and isa.I then
      if isa.Zicsr then
        return inst_set_RV_Base & inst_set_RV32_Base & inst_set_RV_Zicsr & inst_set_RV_Priv;
      else
        return inst_set_RV_Base & inst_set_RV32_Base & inst_set_RV_Priv;
      end if;
    elsif isa.XLEN = 64 and isa.I then
      if isa.Zicsr then
        return inst_set_RV_Base & inst_set_RV64_Base & inst_set_RV_Zicsr & inst_set_RV_Priv;
      else
        return inst_set_RV_Base & inst_set_RV64_Base & inst_set_RV_Priv;
      end if;
    end if;
    assert false
      report "bad instruction set" severity failure;
  end function inst_set;

  --------------------------------------------------------------------------------
  -- compressed instructions

  subtype c_inst_t is std_ulogic_vector(15 downto 0);

  type c_opcode_t is (
  --  | 000     | 001  | 010 | 011         | 100        | 101  | 110 | 111  | <-- inst[15:13]
        ADDI4SPN, FLD,   LW,   FLW,          RSVD,        FSD,   SW,   FSW,   -- 00 } inst[1:0]
        ADDI,     JAL,   LI,   LUI_ADDI16SP, MISC_ALU,    J,     BEQZ, BNEZ,  -- 01 }
        SLLI,     FLDSP, LWSP, FLWSP,        JALR_MV_ADD, FSDSP, SWSP, FSWSP  -- 10 }
  );

  type c_mnemonic_t is (
  -- unknown
    C_ILLEGAL,
  -- Quadrant 0
    C_ADDI4SPN, C_FLD, C_LQ, C_LW, C_FLW, C_LD, C_FSD, C_SQ, C_SW, C_FSW, C_SD,
  -- Quadrant 1
    C_NOP, C_ADDI, C_JAL, C_ADDIW, C_LI, C_ADDI16SP, C_LUI,
    C_SRLI, C_SRLI64, C_SRAI, C_SRAI64, C_ANDI,
    C_SUB, C_XOR, C_OR, C_AND, C_SUBW, C_ADDW,
    C_J, C_BEQZ, C_BNEZ,
  -- Quadrant 2
    C_SLLI, C_SLLI64,
    C_FLDSP, C_LQSP, C_LWSP, C_FLWSP, C_LDSP,
    C_JR, C_MV, C_EBREAK, C_JALR, C_ADD,
    C_FSDSP, C_SQSP, C_SWSP, C_FSWSP, C_SDSP
  );

  type map_rs1_t is (na,RS1,RS1C,x0,x2);
  type map_rs2_t is (na,RS2,RS2C,x0);
  type map_rd_t  is (na,RD,RDC,RDCA,x0,x1,x2);
  type map_imm_t is (na,SHAMT,U5_4,U5_8,U6_2,U6_4,U6_8,S6,S6_16,S6_U,U8_4,S8_2,S10_2);
  type c_inst_def_t is record
    c_mnemonic : c_mnemonic_t;
    pattern    : c_inst_t;
    mnemonic   : mnemonic_t;
    rs1        : map_rs1_t;
    rs2        : map_rs2_t;
    rd         : map_rd_t;
    imm        : map_imm_t;
  end record c_inst_def_t;
  type c_inst_set_t  is array(natural range <>) of c_inst_def_t;

  subtype f1_t is std_ulogic;
  subtype f2_t is std_ulogic_vector(1 downto 0);
  subtype imm6_t is std_ulogic_vector(5 downto 0);

  -- convert c_opcode_t to 5 bit opcode vector
  function rvc_opvec(op : c_opcode_t) return opvec_t is
  begin
    return std_ulogic_vector(to_unsigned(c_opcode_t'pos(op),5));
  end function rvc_opvec;

  -- return ms 2 bits of 5 bit opcode vector
  function rvc_opvec_ms(op : c_opcode_t) return std_ulogic_vector is
  begin
    return rvc_opvec(op)(4 downto 3);
  end function rvc_opvec_ms;

  -- return ls 3 bits of 5 bit opcode vector
  function rvc_opvec_ls(op : c_opcode_t) return std_ulogic_vector is
  begin
    return rvc_opvec(op)(2 downto 0);
  end function rvc_opvec_ls;

  -- CR format with f1
  function CR_fmt(op : c_opcode_t; f1 : f1_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & f1 & "-----" & "-----" & rvc_opvec_ms(op);
  end function CR_fmt;

  -- CR format with f1 and 2 registers
  function CR_fmt(op : c_opcode_t; f1 : f1_t; r1,r2 : xsel_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & f1 & r1 & r2 & rvc_opvec_ms(op);
  end function CR_fmt;

  -- CI format
  function CI_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-----------" & rvc_opvec_ms(op);
  end function CI_fmt;

  -- CI format with register
  function CI_fmt(op : c_opcode_t; r : xsel_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-" & r & "-----" & rvc_opvec_ms(op);
  end function CI_fmt;

  -- CI format with register and immediate
  function CI_fmt(op : c_opcode_t; r : xsel_t; imm6 : imm6_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & imm6(5) & r & imm6(4 downto 0) & rvc_opvec_ms(op);
  end function CI_fmt;

--  -- CI format with immediate
--  function CI_fmt(op : c_opcode_t; imm6 : imm6_t) return c_inst_t is
--  begin
--    return rvc_opvec_ls(op) & imm6(5) & "-----" & imm6(4 downto 0) & rvc_opvec_ms(op);
--  end function CI_fmt;

  -- CSS format
  function CSS_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "------" & "-----" & rvc_opvec_ms(op);
  end function CSS_fmt;

  -- CIW format
  function CIW_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "--------" & "---" & rvc_opvec_ms(op);
  end function CIW_fmt;

  -- CL format
  function CL_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-----------" & rvc_opvec_ms(op);
  end function CL_fmt;

  -- CS format
  function CS_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-----------" & rvc_opvec_ms(op);
  end function CS_fmt;

  -- CJ format
  function CJ_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-----------" & rvc_opvec_ms(op);
  end function CJ_fmt;

  -- CB format
  function CB_fmt(op : c_opcode_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-----------" & rvc_opvec_ms(op);
  end function CB_fmt;

  -- CB format with f2 and immediate
  function CB_fmt(op : c_opcode_t; f2 : f2_t; imm6 : imm6_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & imm6(5) & f2 & "---" & imm6(4 downto 0) & rvc_opvec_ms(op);
  end function CB_fmt;

  -- CB format with f2
  function CB_fmt(op : c_opcode_t; f2 : f2_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & "-" & f2 & "---" & "-----" & rvc_opvec_ms(op);
  end function CB_fmt;

  -- CA format
  function CA_fmt(op : c_opcode_t; f1 : f1_t; f2b : f2_t) return c_inst_t is
  begin
    return rvc_opvec_ls(op) & f1 & "11" & "---" & f2b & "---" & rvc_opvec_ms(op);
  end function CA_fmt;

  constant f1_0    : f1_t := '0';
  constant f1_1    : f1_t := '0';
  constant f2_srli : f2_t := "00";
  constant f2_srai : f2_t := "01";
  constant f2_andi : f2_t := "10";

  constant f2b_sub  : f2_t := "00";
  constant f2b_xor  : f2_t := "01";
  constant f2b_or   : f2_t := "10";
  constant f2b_and  : f2_t := "11";
  constant f2b_subw : f2_t := "00";
  constant f2b_addw : f2_t := "01";

  constant c_inst_set_RVC : c_inst_set_t := (
    ( C_ADDI4SPN , CIW_fmt ( ADDI4SPN                                 ) , U_ADDI   , x2   , na   , RDC  , U8_4  ), -- addi rd′, x2, nzuimm[9:2]
    ( C_LW       , CL_fmt  ( LW                                       ) , U_LW     , RS1C , na   , RDC  , U5_4  ), -- lw rd′, offset[6:2](rs1′)
    ( C_SW       , CS_fmt  ( SW                                       ) , U_SW     , RS1C , RS2C , na   , U5_4  ), -- sw rs2 ′,offset[6:2](rs1′)
    ( C_NOP      , CI_fmt  ( ADDI         , "00000" , "000000"        ) , U_ADDI   , x0   , na   , x0   , na    ), -- addi rd, rd, nzimm[5:0]
    ( C_ADDI     , CI_fmt  ( ADDI                                     ) , U_ADDI   , RS1  , na   , RD   , S6    ), -- addi rd, rd, nzimm[5:0]
    ( C_LI       , CI_fmt  ( LI                                       ) , U_ADDI   , RS1  , x0   , RD   , S6    ), -- addi rd, x0, imm[5:0]
    ( C_ADDI16SP , CI_fmt  ( LUI_ADDI16SP , "00010"                   ) , U_ADDI   , x2   , na   , x2   , S6_16 ), -- addi x2, x2,nzimm[9:4]
    ( C_LUI      , CI_fmt  ( LUI_ADDI16SP                             ) , U_LUI    , na   , na   , RD   , S6_U  ), -- lui rd, nzimm[17:12]
    ( C_SRLI     , CB_fmt  ( MISC_ALU            , f2_srli , "0-----" ) , U_SRLI   , RS1C , na   , RDCA , SHAMT ), -- srli rd′, rd′, shamt[5:0]
    ( C_SRAI     , CB_fmt  ( MISC_ALU            , f2_srai , "0-----" ) , U_SRAI   , RS1C , na   , RDCA , SHAMT ), -- srai rd′, rd′, shamt[5:0]
    ( C_ANDI     , CB_fmt  ( MISC_ALU            , f2_andi            ) , U_ANDI   , RS1C , na   , RDCA , S6    ), -- andi rd′, rd′, imm[5:0]
    ( C_SUB      , CA_fmt  ( MISC_ALU     , f1_0 , f2b_sub            ) , U_SUB    , RS1C , RS2C , RDCA , na    ), -- sub rd′, rd′, rs2′.
    ( C_XOR      , CA_fmt  ( MISC_ALU     , f1_0 , f2b_xor            ) , U_XOR    , RS1C , RS2C , RDCA , na    ), -- xor rd′, rd′, rs2′
    ( C_OR       , CA_fmt  ( MISC_ALU     , f1_0 , f2b_or             ) , U_OR     , RS1C , RS2C , RDCA , na    ), -- or rd′, rd′, rs2′
    ( C_AND      , CA_fmt  ( MISC_ALU     , f1_0 , f2b_and            ) , U_AND    , RS1C , RS2C , RDCA , na    ), -- and rd′, rd′, rs2′
    ( C_J        , CJ_fmt  ( J                                        ) , U_JAL    , x0   , na   , x0   , S10_2 ), -- jal x0, offset[11:1]
    ( C_BEQZ     , CB_fmt  ( BEQZ                                     ) , U_BEQ    , RS1C , x0   , na   , S8_2  ), -- beq rs1′, x0, offset[8:1]
    ( C_BNEZ     , CB_fmt  ( BNEZ                                     ) , U_BNE    , RS1C , x0   , na   , S8_2  ), -- bne rs1′, x0, offset[8:1]
    ( C_SLLI     , CI_fmt  ( SLLI                                     ) , U_SLLI   , RS1  , na   , RD   , SHAMT ), -- slli rd, rd, shamt[5:0]
    ( C_LWSP     , CI_fmt  ( LWSP                                     ) , U_LW     , x2   , na   , RD   , U6_4  ), -- lw rd, offset[7:2](x2)
    ( C_JR       , CR_fmt  ( JALR_MV_ADD  , f1_0 , "XXXXX" , "00000"  ) , U_JALR   , RS1  , na   , x0   , na    ), -- jalr x0, 0(rs1)
    ( C_MV       , CR_fmt  ( JALR_MV_ADD  , f1_0                      ) , U_ADD    , RS1  , x0   , RD   , na    ), -- add rd, x0, rs2
    ( C_EBREAK   , CR_fmt  ( JALR_MV_ADD  , f1_1 , "00000" , "00000"  ) , U_EBREAK , RS1  , na   , na   , na    ), -- ebreak
    ( C_JALR     , CR_fmt  ( JALR_MV_ADD  , f1_1 , "XXXXX" , "00000"  ) , U_JALR   , RS1  , na   , x1   , na    ), -- jalr x1, 0(rs1)
    ( C_ADD      , CR_fmt  ( JALR_MV_ADD  , f1_1                      ) , U_ADD    , RS1  , RS2  , RD   , na    ), -- add rd, rd, rs2
    ( C_SWSP     , CSS_fmt ( SWSP                                     ) , U_SW     , x2   , RS2  , na   , U6_4  )  -- sw rs2, offset[7:2](x2)
  );

  constant c_inst_set_RV32C : c_inst_set_t := ( 0 =>
    ( C_JAL      , CJ_fmt  ( JAL                                      ) , U_JAL    , na   , na   , x1   , S10_2 )  -- jal x1, offset[11:1]
  );

  constant c_inst_set_RV64C : c_inst_set_t := (
    ( C_LD      , CL_fmt  ( FLW                                       ) , U_LD     , RS1C , na   , RDC  , U5_8  ),
    ( C_SD      , CS_fmt  ( FSW                                       ) , U_SD     , RS1C , RS2C , na   , U5_8  ),
    ( C_ADDIW   , CI_fmt  ( JAL                                       ) , U_ADDIW  , RS1  , na   , RD   , S6    ),
    ( C_SUBW    , CA_fmt  ( MISC_ALU     , f1_1 , f2b_subw            ) , U_SUBW   , RS1C , RS2C , RDCA , na    ),
    ( C_ADDW    , CA_fmt  ( MISC_ALU     , f1_1 , f2b_addw            ) , U_ADDW   , RS1C , RS2C , RDCA , na    ),
    ( C_LDSP    , CI_fmt  ( FLWSP                                     ) , U_LD     , x2   , na   , RD   , U6_8  ),
    ( C_SDSP    , CI_fmt  ( FSWSP                                     ) , U_SD     , x2   , RS2  , na   , U6_8  )
  );

--  constant c_inst_set_RV32FC : c_inst_set_t := c_inst_set_RV32C + (
--    ( C_FLW      , CL_fmt  ( FLW                                      ) ,             ,      ,      ,      ,       ),
--    ( C_FSW      , CS_fmt  ( FSW                                      ) ,             ,      ,      ,      ,       ),
--    ( C_FLWSP    , CI_fmt  ( FLWSP                                    ) ,             ,      ,      ,      ,       ),
--    ( C_FSWSP    , CI_fmt  ( FSWSP                                    ) ,             ,      ,      ,      ,       )
--  );
--
--  constant c_inst_set_RV32DC : c_inst_set_t := c_inst_set_RV32FC + (
--    ( C_FLD     , CL_fmt  ( FLD                                       ) ,              ,      ,      ,      ,       ),
--    ( C_FSD     , CS_fmt  ( FSD                                       ) ,              ,      ,      ,      ,       ),
--    ( C_FLDSP   , CI_fmt  ( FLDSP                                     ) ,              ,      ,      ,      ,       ),
--    ( C_FSDSP   , CI_fmt  ( FSDSP                                     ) ,              ,      ,      ,      ,       )
--  );

--  constant c_inst_set_RV64FC : c_inst_set_t := c_inst_set_RV64C + (
--    ( C_LD      , CL_fmt  ( FLW                                       ) ,              ,      ,      ,      ,       ),
--    ( C_SD      , CS_fmt  ( FSW                                       ) ,              ,      ,      ,      ,       )
--  );
--
--  constant c_inst_set_RV64DC : c_inst_set_t := c_inst_set_RV64FC + (
--    ( C_FLD      , CL_fmt  ( FLD                                      ) ,              ,      ,      ,      ,       ),
--    ( C_FSD      , CS_fmt  ( FSD                                      ) ,              ,      ,      ,      ,       ),
--    ( C_FLDSP    , CI_fmt  ( FLDSP                                    ) ,              ,      ,      ,      ,       ),
--    ( C_FSDSP    , CI_fmt  ( FSDSP                                    ) ,              ,      ,      ,      ,       )
--  );

-- RV128 not supported at present
--  constant c_inst_set_RV128C : c_inst_set_t := c_inst_set_RVC + (
--    ( C_LQ        , CL_fmt  ( FLD                                     ) ,              ,      ,      ,      ,       ),
--    ( C_SQ        , CS_fmt  ( FSD                                     ) ,              ,      ,      ,      ,       ),
--    ( C_SRLI64    , CB_fmt  ( MISC_ALU     , f2_srli       , "000000" ) ,              ,      ,      ,      ,       ),
--    ( C_SRAI64    , CB_fmt  ( MISC_ALU     , f2_srai       , "000000" ) ,              ,      ,      ,      ,       ),
--    ( C_SLLI64    , CI_fmt  ( SLLI                         , "000000" ) ,              ,      ,      ,      ,       ),
--    ( C_LQSP      , CI_fmt  ( FLDSP                                   ) ,              ,      ,      ,      ,       ),
--    ( C_SQSP      , CI_fmt  ( FSDSP                                   ) ,              ,      ,      ,      ,       )
--  );

  function c_inst_set(isa : isa_t) return c_inst_set_t is
  begin
    -- TODO handle RV64, F, D
    if isa.XLEN = 32 then
      return c_inst_set_RVC & c_inst_set_RV32C;
    elsif isa.XLEN = 64 then
      return c_inst_set_RVC & c_inst_set_RV64C;
    end if;
  end function c_inst_set;

  function uncompress(u : inst_t; isa : isa_t) return inst_t is
    constant cset : c_inst_set_t := c_inst_set(isa);
    constant iset : inst_set_t := inst_set(isa);
    variable cdef : c_inst_def_t;
    variable idef : inst_def_t;
    variable b    : std_ulogic_vector(12 downto 1);
    variable j    : std_ulogic_vector(20 downto 1);
    variable r    : inst_t;
  begin
    r := u;
    if isa.C and u(1 downto 0) /= "11" then
      loop_x: for x in 0 to cset'length-1 loop
        cdef := cset(x);
        if pattern_match(u(15 downto 0),cdef.pattern) then
          loop_y: for y in 0 to iset'length-1 loop
            idef := iset(y);
            if cdef.mnemonic = idef.mnemonic then
              r := idef.pattern;
              with cdef.rs1 select r(19 downto 15) :=
                              "XXXXX" when na,
                       u(11 downto 7) when RS1,
                "01" & u( 9 downto 7) when RS1C,
                              "00000" when x0,
                              "00010" when x2;
              with cdef.rs2 select r(24 downto 20) :=
                              "XXXXX" when na,
                        u(6 downto 2) when RS2,
                 "01" & u(4 downto 2) when RS2C,
                              "00000" when x0;
              with cdef.rd select r(11 downto  7) :=
                              "XXXXX" when na,
                       u(11 downto 7) when RD,
                "01" & u( 4 downto 2) when RDC,
                "01" & u( 9 downto 7) when RDCA,
                              "00000" when x0,
                              "00001" when x1,
                              "00010" when x2;
              case cdef.imm is
                when SHAMT  => -- C.SRLI, C.SRAI
                  r(24 downto 20) := u(6 downto 2);
                when U5_4   => -- C.LW
                  r(31 downto 20) := "00000" & u(5) & u(12 downto 10) & u(6) & "00";
                when U6_4   => -- C.LWSP
                  r(31 downto 20) := "0000" & u(3 downto 2) & u(12) & u(6 downto 4) & "00";
                when S6     => -- C.ADDI etc
                  r(31 downto 20) := std_ulogic_vector(resize(signed(u(12) & u(6 downto 2)),12));
                when S6_16  => -- C.ADDI16SP
                  r(31 downto 20) := std_ulogic_vector(resize(signed(u(12) & u(6 downto 2)) & "0000",12));
                when S6_U   => -- C.LUI
                  r(31 downto 12) := (31 downto 18 => u(12), 17 downto 12 => u(12) & u(6 downto 2));
                when U8_4   => -- C.ADDI4SPN
                  r(31 downto 20) := "00" & u(10 downto 7) & u(12 downto 11) & u(5) & u(6) & "00";
                when S8_2   => -- C.BEQ, C.BNE
                  b := (12 downto 9 => u(12), 8 downto 1 => u(12) & u(6 downto 5) & u(2) & u(11 downto 10) & u(4 downto 3));
                  r(31) := b(12); r(7) := b(11); r(30 downto 25) := b(10 downto 5); r(11 downto 8) := b(4 downto 1);
                when S10_2  => -- C.JAL
                  j := (20 downto 12 => u(12), 11 downto 1 => u(12) & u(8) & u(10 downto 9) & u(6) & u(7) & u(2) & u(11) & u(5 downto 3));
                  r(31 downto 12) := j(20) & j(10 downto 1) & j(11) & j(19 downto 12);
                when others => null;
              end case;
              exit loop_x;
            end if;
          end loop loop_y;
          exit;
        end if;
      end loop loop_x;
    end if;
    return r;
  end function uncompress;

  --------------------------------------------------------------------------------

end package body isa_pkg;

--------------------------------------------------------------------------------