--------------------------------------------------------------------------------
-- mcu3.vhd                                                                   --
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
-- RISC-V microcontroller core with 3 stage pipeline
-- RV32I or RV64I base ISA, M mode only
-- Optional ISA extensions:
--  Zicsr (control and status register instructions)
--  C (compressed instructions)

use work.common_pkg.all;
use work.isa_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;

package mcu3_pkg is

  component mcu3 is
    generic (
      isa_name   : string := "RV32ICZicsr";
      rstvec     : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
      mtvec      : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0)
    );
    port (

      rst       : in    std_ulogic;
      clk       : in    std_ulogic;
      clken     : in    std_ulogic;

      irq       : in    std_ulogic;

      mtime     : in    tval_t;
      mtimecmp  : in    tval_t;

      if_avalid : out   std_ulogic;
      if_ajmp   : out   std_ulogic;
      if_aaddr  : out   std_ulogic_vector;
      if_amx    : in    std_ulogic;
      if_aready : in    std_ulogic;

      if_rvalid : in    std_ulogic;
      if_rdata  : in    std_ulogic_vector;
      if_rready : out   std_ulogic;

      ls_avalid : out   std_ulogic;
      ls_aaddr  : out   std_ulogic_vector;
      ls_amx    : in    std_ulogic;
      ls_aready : in    std_ulogic;

      ls_wvalid : out   std_ulogic;
      ls_wsize  : out   sz_t;
      ls_wdata  : out   std_ulogic_vector;
      ls_wready : in    std_ulogic;

      ls_rvalid : in    std_ulogic;
      ls_rdata  : in    std_ulogic_vector;
      ls_rready : out   std_ulogic;

      csr_x       : in    std_ulogic;
      csr_en      : out   std_ulogic;
      csr_wop     : out   csr_wop_t;
      csr_sel     : out   csra_t;
      csr_wdata   : out   std_ulogic_vector;
      csr_rdata_x : in    std_ulogic_vector
    );
  end component mcu3;

end package mcu3_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;
use work.isa_pkg.all;
use work.mcu3_pkg.all;

use work.decoder_pkg.all;
use work.regfile_ad_pkg.all;
use work.alu_pkg.all;
use work.csr_m_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity mcu3 is
  generic (
    isa_name   : string := "RV32ICZicsr";
    rstvec     : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
    mtvec      : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0)
  );

  port (

    rst       : in    std_ulogic;
    clk       : in    std_ulogic;
    clken     : in    std_ulogic;

    irq       : in    std_ulogic;

    mtime     : in    tval_t;
    mtimecmp  : in    tval_t;

    -- instruction fetch address channel
    if_avalid : out   std_ulogic;                                              -- instruction address valid
    if_ajmp   : out   std_ulogic;                                              -- reset/exception/jump/branch
    if_aaddr  : out   std_ulogic_vector; -- instruction address
    if_amx    : in    std_ulogic;                                              -- load/store misalign exceptions
    if_aready : in    std_ulogic;                                              -- instruction address ready

    -- instruction fetch read data channel
    if_rvalid : in    std_ulogic;                                              -- instruction data valid
    if_rdata  : in    std_ulogic_vector; -- instruction data
    if_rready : out   std_ulogic;                                              -- instruction data ready

    -- load/store address channel
    ls_avalid : out   std_ulogic;                                              -- load/store address valid
    ls_aaddr  : out   std_ulogic_vector; -- load/store address
    ls_amx    : in    std_ulogic;                                              -- load/store misalign exceptions
    ls_aready : in    std_ulogic;                                              -- load/store address ready

    -- load/store write data channel
    ls_wvalid : out   std_ulogic;                                              -- load/store write data valid
    ls_wsize  : out   sz_t;                                                    -- load/store size
    ls_wdata  : out   std_ulogic_vector; -- load/store write data
    ls_wready : in    std_ulogic;                                              -- load/store write data ready

    -- load/store read data channel
    ls_rvalid : in    std_ulogic;                                              -- load/store read data valid
    ls_rdata  : in    std_ulogic_vector; -- load/store read data
    ls_rready : out   std_ulogic;                                              -- load/store read data ready

    -- CSR (to support verification)
    csr_x       : in    std_ulogic;
    csr_en      : out   std_ulogic;
    csr_wop     : out   csr_wop_t;
    csr_sel     : out   csra_t;
    csr_wdata   : out   std_ulogic_vector;
    csr_rdata_x : in    std_ulogic_vector

  );
end entity mcu3;

architecture rtl of mcu3 is

  function isa_check(isa_name : string) return isa_t is
    variable r : isa_t;
  begin
    r := isa_decode(isa_name);
    assert r.I and not r.E and (r.XLEN = 32 or r.XLEN = 64)
      report "unsupported base ISA" severity failure;
    assert not r.M
      report "M extension not yet supported" severity failure;
    assert not r.A
      report "A extension not yet supported" severity failure;
    assert r.F nor r.D
      report "floating point extensions not yet supported" severity failure;
    assert r.Zicsr
      report lf & "ISA decoded:" & lf &
        "  XLEN     : " & integer'image(r.XLEN) & lf &
        "  I        : " & boolean'image(r.I) & lf &
        "  E        : " & boolean'image(r.E) & lf &
        "  M        : " & boolean'image(r.M) & lf &
        "  A        : " & boolean'image(r.A) & lf &
        "  F        : " & boolean'image(r.F) & lf &
        "  D        : " & boolean'image(r.D) & lf &
        "  C        : " & boolean'image(r.C) & lf &
        "  Zicsr    : " & boolean'image(r.Zicsr) & lf &
        "  Zifencei : " & boolean'image(r.Zifencei) & lf &
      "Zicsr extension is required" severity failure;
    assert not r.Zifencei
      report "Zifencei extension not yet supported" severity failure;
    return r;
  end function isa_check;

  constant isa : isa_t := isa_check(isa_name);

  subtype xval_t is std_ulogic_vector(isa.XLEN-1 downto 0);

  type s1_t is record
    tag     : tag_t;
    rst     : std_ulogic;
    if_done : std_ulogic;
    branch  : std_ulogic;
    rdyo    : std_ulogic;
  end record s1_t;

  type s2_t is record
    tag     : tag_t;
    busy    : std_ulogic;
    imx     : std_ulogic;
    pc      : xval_t;
    excep   : std_ulogic;
    valid   : std_ulogic;
    valid_s : std_ulogic;
    uinst   : inst_t;
    uinst_s : inst_t;
    c       : std_ulogic;
    tinst   : inst_t;
    ls_done : std_ulogic;
    rdyi    : std_ulogic;
    rdyo    : std_ulogic;
  end record s2_t;

  type s3_t is record
    tag     : tag_t;
    busy    : std_ulogic;
    pc      : xval_t;
    rdyi    : std_ulogic;
    rdyo    : std_ulogic;
  end record s3_t;

  signal irq_e       : std_ulogic;
  signal irq_t       : std_ulogic;

  signal s1          : s1_t;
  signal s2          : s2_t;
  signal s3          : s3_t;

  signal d           : d_t;

  signal rs1_sel     : xsel_t;
  signal rs1_data    : xval_t;
  signal rs2_sel     : xsel_t;
  signal rs2_data    : xval_t;
  signal rd_en       : std_ulogic;
  signal rd_ld       : std_ulogic;
  signal rd_sz       : ssz_t;
  signal rd_sel      : xsel_t;
  signal rd_data     : xval_t;
  signal ld_rdy      : std_ulogic;

  signal alu_eq      : std_ulogic;
  signal alu_lt      : std_ulogic;
  signal alu_ltu     : std_ulogic;
  signal alu_rd      : xval_t;

  signal csr         : csr_m_t (
      mvendorid  (isa.XLEN-1 downto 0),
      marchid    (isa.XLEN-1 downto 0),
      mimpid     (isa.XLEN-1 downto 0),
      mhartid    (isa.XLEN-1 downto 0),
      mconfigptr (isa.XLEN-1 downto 0),
      misa       (isa.XLEN-1 downto 0),
      mie        (isa.XLEN-1 downto 0),
      mtvec      (isa.XLEN-1 downto 0),
      mscratch   (isa.XLEN-1 downto 0),
      mepc       (isa.XLEN-1 downto 0),
      mcause     (isa.XLEN-1 downto 0),
      mtval      (isa.XLEN-1 downto 0),
      mip        (isa.XLEN-1 downto 0),
      mtinst     (isa.XLEN-1 downto 0),
      mtval2     (isa.XLEN-1 downto 0)
    );

  signal csr_nonex   : std_ulogic;
  signal csr_rdata_i : xval_t;
  signal csr_rdata   : xval_t;

  signal exceps      : exceps_t;
  signal cause       : xval_t;
  signal tval        : xval_t;

begin

  --------------------------------------------------------------------------------
  -- stage 1 : fetch

  P_STAGE1_S: process(rst,clk)
  begin
    if rst = '1' then
      s1.tag     <= (others => '0');
      s1.rst     <= '1';
      s1.if_done <= '0';
    elsif rising_edge(clk) and clken = '1' then
      if s1.rdyo and s2.rdyi then
        s1.tag     <= std_ulogic_vector(unsigned(s1.tag)+1);
        s1.rst     <= '0';
        s1.if_done <= '0';
      elsif if_avalid and if_aready then
        s1.if_done <= '1';
      end if;
    end if;
  end process P_STAGE1_S;

  P_STAGE1_A: process(all)
  begin
    if_avalid <= not rst and (s1.rst or s2.valid) and not s1.if_done;
    s1.branch <= '1' when
        (d.mnemonic = U_BEQ  and alu_eq  = '1') or
        (d.mnemonic = U_BNE  and alu_eq  = '0') or
        (d.mnemonic = U_BLT  and alu_lt  = '1') or
        (d.mnemonic = U_BGE  and alu_lt  = '0') or
        (d.mnemonic = U_BLTU and alu_ltu = '1') or
        (d.mnemonic = U_BGEU and alu_ltu = '0')
      else '0';
    if_ajmp   <= '1';
    if         s1.rst = '1'     then if_aaddr <= rstvec(isa.XLEN-1 downto 2) & "00";
    elsif    s2.excep = '1'     then if_aaddr <= csr.mtvec(isa.XLEN-1 downto 2) & "00";
    elsif  d.mnemonic = U_MRET  then if_aaddr <= csr.mepc(isa.XLEN-1 downto 0);
    elsif    d.opcode = JAL     then if_aaddr <= std_ulogic_vector(signed(s2.pc)+signed(d.imm20 & '0'));
    elsif    d.opcode = JALR    then if_aaddr <= std_ulogic_vector(signed(rs1_data)+signed(d.imm12)); if_aaddr(0) <= '0';
    elsif   s1.branch = '1'     then if_aaddr <= std_ulogic_vector(signed(s2.pc) + signed(d.imm12b & '0'));
    else
      if_ajmp <= '0';
      if s2.c then
        if_aaddr <= std_ulogic_vector(unsigned(s2.pc)+2);
      else
        if_aaddr <= std_ulogic_vector(unsigned(s2.pc)+4);
      end if;
    end if;
    s1.rdyo <= (if_avalid and if_aready) or s1.if_done;
  end process P_STAGE1_A;

  --------------------------------------------------------------------------------
  -- stage 2 : execute (decode, read registers, execute, writeback)
  -- wait for instruction
  -- wait for load/store address (and write data)

  P_STAGE2_S: process(rst,clk)
  begin
    if rst = '1' then
      s2.tag     <= (others => '1');
      s2.busy    <= '0';
      s2.imx     <= '0';
      s2.pc      <= (others => 'X');
      s2.valid_s <= '0';
      s2.uinst_s <= (others => 'X');
      s2.ls_done <= '0';
    elsif rising_edge(clk) and clken = '1' then
      if if_rvalid and if_rready then
        s2.imx     <= if_amx;
        s2.valid_s <= '1';
        s2.uinst_s <= if_rdata;
      end if;
      if ls_avalid and ls_aready then
        s2.ls_done <= '1';
      end if;
      if s1.rdyo and s2.rdyi then
        s2.tag     <= s1.tag;
        s2.busy    <= '1';
        s2.imx     <= '0';
        s2.pc      <= if_aaddr;
        s2.valid_s <= '0';
        s2.uinst_s <= (others => 'X');
      elsif s2.rdyo and s3.rdyi then
        s2.busy    <= '0';
        s2.valid_s <= '0';
      end if;
    end if;
  end process P_STAGE2_S;

  P_STAGE2_A: process(all)
  begin
    s2.c      <= bool2sl(s2.uinst(1 downto 0) /= "11");
    s2.valid  <= (if_rvalid and if_rready) or s2.valid_s;
    s2.excep  <= bool2sl(exceps /= (others => '0'));
    if_rready <= not s2.valid_s;
    s2.uinst  <= if_rdata when if_rvalid and if_rready else s2.uinst_s;
    rd_en     <= s2.valid and d.rd_en and not s2.valid_s;
    rd_ld     <= bool2sl(d.opcode = LOAD);
    csr_en    <= s2.valid and d.csr_en and not s2.valid_s;
    ls_avalid <= s2.valid and bool2sl(d.opcode = LOAD or d.opcode = STORE) and not s2.ls_done;
    ls_aaddr  <= std_ulogic_vector(signed(rs1_data)+signed(d.imm12));
    ls_wvalid <= s2.valid and bool2sl(d.opcode = STORE) and not s2.ls_done;
    ls_wsize  <= d.f3(1 downto 0);
    ls_wdata  <= rs2_data;
    s2.rdyi   <= (s2.rdyo and s3.rdyi) or not s2.busy;
    s2.rdyo   <= s2.valid and not (ls_avalid and not ls_aready) and not (ls_wvalid and not ls_wready);
  end process P_STAGE2_A;

  --------------------------------------------------------------------------------
  -- stage 3 : complete (wait for load data, retire)

  P_STAGE3_S: process(rst,clk)
  begin
    if rst = '1' then
      s3.tag    <= (others => '1');
      s3.busy   <= '0';
      ls_rready <= '0';
    elsif rising_edge(clk) then
      if s2.rdyo and s3.rdyi then
        s3.tag    <= s2.tag;
        s3.busy   <= '1';
        s3.pc     <= if_aaddr;
        ls_rready <= (ls_avalid or s2.ls_done) and bool2sl(d.opcode = LOAD);
      elsif s3.rdyo then
        s3.busy   <= '0';
        ls_rready <= '0';
      end if;
    end if;
  end process P_STAGE3_S;

  P_STAGE3_A: process(all)
  begin
    s3.rdyi <= not (ls_rready and not ls_rvalid);
    s3.rdyo <= s3.busy and not (ls_rready and not ls_rvalid);
  end process P_STAGE3_A;

  --------------------------------------------------------------------------------

  U_DECODER: component decoder
    generic map (
      isa   => isa
    )
    port map (
      valid => s2.valid,
      i     => s2.uinst, -- untransformed instruction
      o     => s2.tinst, -- transformed (uncompressed) instruction
      d     => d
    );

  --------------------------------------------------------------------------------

  rs1_sel  <= d.rs1_sel;
  rs2_sel  <= d.rs2_sel;
  rd_sz    <= d.rd_sz;
  rd_sel   <= d.rd_sel;
  rd_data  <= csr_rdata when d.opcode = SYSTEM else alu_rd;
  ld_rdy   <= ls_rvalid and ls_rready;

  U_REGFILE: component regfile_ad
    generic map (
      isa       => isa
    )
    port map (
      rst       => rst,
      clk       => clk,
      clken     => clken,
      rd_en     => rd_en,
      rd_ld     => rd_ld,
      rd_sz     => rd_sz,
      rd_sel    => rd_sel,
      rd_data   => rd_data,
      ld_data   => ls_rdata,
      ld_rdy    => ld_rdy,
      rs1_sel   => rs1_sel,
      rs1_data  => rs1_data,
      rs2_sel   => rs2_sel,
      rs2_data  => rs2_data
    );

  --------------------------------------------------------------------------------

  U_ALU: component alu
    generic map (
      isa   => isa
    )
    port map (
      inst  => s2.tinst,
      pc    => s2.pc,
      rs1   => rs1_data,
      rs2   => rs2_data,
      rd    => alu_rd,
      eq    => alu_eq,
      lt    => alu_lt,
      ltu   => alu_ltu
    );

  --------------------------------------------------------------------------------
  -- CSRs

  csr_sel   <= d.csr_sel;
  csr_wop   <= d.csr_wop;
  csr_wdata <= rs1_data;
  csr_rdata <= csr_rdata_x when csr_x else csr_rdata_i;

  U_CSR: component csr_m
    generic map (
      isa    => isa,
      mtvec  => mtvec
    )
    port map (
      rst     => rst,
      clk     => clk,
      clken   => clken,
      csr     => csr,
      en      => csr_en,
      wop     => csr_wop,
      sel     => csr_sel,
      wdata   => csr_wdata,
      rdata   => csr_rdata_i,
      nonex   => csr_nonex,
      instret => s3.rdyo,
      mtime   => mtime,
      excep   => s2.excep,
      irq_e   => irq_e,
      irq_t   => irq_t,
      pc      => s2.pc,
      cause   => cause,
      tval    => tval,
      tinst   => s2.tinst
    );

  --------------------------------------------------------------------------------
  -- exceptions (interrupts and traps)

  P_IRQ: process(rst,clk)
  begin
    if rst = '1' then
      irq_e  <= '0';
      irq_t  <= '0';
    elsif rising_edge(clk) and clken = '1' then
      irq_e <= irq;
      irq_t <= bool2sl(mtime >= mtimecmp);
    end if;
  end process P_IRQ;

  P_EXCEP: process(all)
  begin

    exceps.interrupt_s_u      <= '0';
    exceps.interrupt_s_s      <= '0';
    exceps.interrupt_s_h      <= '0';
    exceps.interrupt_s_m      <= '0';
    exceps.interrupt_t_u      <= '0';
    exceps.interrupt_t_s      <= '0';
    exceps.interrupt_t_h      <= '0';
    exceps.interrupt_t_m      <= csr.mstatus(CSRB_MIP_MTIP) and csr.mie(CSRB_MIE_MTIE) and csr.mstatus(CSRB_MSTATUS_MIE);
    exceps.interrupt_e_u      <= '0';
    exceps.interrupt_e_s      <= '0';
    exceps.interrupt_e_h      <= '0';
    exceps.interrupt_e_m      <= csr.mstatus(CSRB_MIP_MEIP) and csr.mie(CSRB_MIE_MEIE) and csr.mstatus(CSRB_MSTATUS_MIE);
    exceps.interrupt_rsvd_12  <= '0';
    exceps.interrupt_rsvd_13  <= '0';
    exceps.interrupt_rsvd_14  <= '0';
    exceps.interrupt_rsvd_15  <= '0';
    exceps.trap_align_instr   <= s2.valid and s2.imx;
    exceps.trap_access_instr  <= '0';
    exceps.trap_illegal_instr <= s2.valid and ((csr_nonex and not csr_x) or not d.legal);
    exceps.trap_break         <= s2.valid and bool2sl(d.mnemonic = U_EBREAK);
    exceps.trap_align_load    <= s2.valid and bool2sl(d.opcode = LOAD) and ls_amx;
    exceps.trap_access_load   <= '0';
    exceps.trap_align_store   <= s2.valid and bool2sl(d.opcode = STORE) and ls_amx;
    exceps.trap_access_store  <= '0';
    exceps.trap_ecall_u       <= '0';
    exceps.trap_ecall_s       <= '0';
    exceps.trap_ecall_h       <= '0';
    exceps.trap_ecall_m       <= s2.valid and bool2sl(d.mnemonic = U_ECALL);
    exceps.trap_page_instr    <= '0';
    exceps.trap_page_load     <= '0';
    exceps.trap_rsvd_14       <= '0';
    exceps.trap_page_store    <= '0';

    cause(31) <= exceps.interrupt_e_m or exceps.interrupt_s_m or exceps.interrupt_t_m;
    cause(30 downto 4) <= (others => '0');
    cause(3 downto 0) <=
      ECAUSE_INTERRUPT_E_M      when exceps.interrupt_e_m      = '1' else
      ECAUSE_INTERRUPT_T_M      when exceps.interrupt_t_m      = '1' else
      ECAUSE_INTERRUPT_S_M      when exceps.interrupt_s_m      = '1' else
      ECAUSE_TRAP_ALIGN_INSTR   when exceps.trap_align_instr   = '1' else
      ECAUSE_TRAP_ILLEGAL_INSTR when exceps.trap_illegal_instr = '1' else
      ECAUSE_TRAP_ALIGN_LOAD    when exceps.trap_align_load    = '1' else
      ECAUSE_TRAP_ALIGN_STORE   when exceps.trap_align_store   = '1' else
      ECAUSE_TRAP_BREAK         when exceps.trap_break         = '1' else
      ECAUSE_TRAP_ECALL_M       when exceps.trap_ecall_m       = '1' else
      (others => 'X');

    if cause(31) = '0' then
      with cause(3 downto 0) select tval <=
        s2.pc           when ECAUSE_TRAP_ALIGN_INSTR | ECAUSE_TRAP_ILLEGAL_INSTR,
        ls_aaddr        when ECAUSE_TRAP_ALIGN_LOAD  | ECAUSE_TRAP_ALIGN_STORE,
        (others => '0') when others;
    else
      tval <= (others => '0');
    end if;

    end process P_EXCEP;

  --------------------------------------------------------------------------------

end architecture rtl;
