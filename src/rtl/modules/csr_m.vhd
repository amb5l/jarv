--------------------------------------------------------------------------------
-- csr_m.vhd                                                                  --
-- JARV minimal/m-mode CSR module.                                            --
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
use work.isa_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;

package csr_m_pkg is

  type csr_m_t is record
    -- unprivileged
    cycle      : std_ulogic_vector(63 downto 0);
    time       : std_ulogic_vector(63 downto 0);
    instret    : std_ulogic_vector(63 downto 0);
    -- privileged - machine level
    mvendorid  : std_ulogic_vector;
    marchid    : std_ulogic_vector;
    mimpid     : std_ulogic_vector;
    mhartid    : std_ulogic_vector;
    mconfigptr : std_ulogic_vector;
    mstatus    : std_ulogic_vector(63 downto 0);
    misa       : std_ulogic_vector;
    mie        : std_ulogic_vector;
    mtvec      : std_ulogic_vector;
    mscratch   : std_ulogic_vector;
    mepc       : std_ulogic_vector;
    mcause     : std_ulogic_vector;
    mtval      : std_ulogic_vector;
    mip        : std_ulogic_vector;
    mtinst     : std_ulogic_vector;
    mtval2     : std_ulogic_vector;
    mcycle     : std_ulogic_vector(63 downto 0);
    minstret   : std_ulogic_vector(63 downto 0);
  end record csr_m_t;

  component csr_m is
    generic (
      isa     : isa_t;
      mtvec   : std_ulogic_vector(isa.XLEN-1 downto 0)
    );
    port (
      rst     : in    std_ulogic;
      clk     : in    std_ulogic;
      clken   : in    std_ulogic;
      csr     : inout csr_m_t;
      en      : in    std_ulogic;
      wop     : in    csr_wop_t;
      sel     : in    csra_t;
      wdata   : in    std_ulogic_vector;
      rdata   : out   std_ulogic_vector;
      nonex   : out   std_ulogic;
      instret : in    std_ulogic;
      mtime   : in    tval_t;
      excep   : in    std_ulogic;
      irq_e   : in    std_ulogic;
      irq_t   : in    std_ulogic;
      pc      : in    std_ulogic_vector;
      cause   : in    std_ulogic_vector;
      tval    : in    std_ulogic_vector;
      tinst   : in    std_ulogic_vector
    );
  end component csr_m;

end package csr_m_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;
use work.isa_pkg.all;
use work.csr_m_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity csr_m is
  generic (
    isa     : isa_t;
    mtvec   : std_ulogic_vector(isa.XLEN-1 downto 0)
  );
  port (

    rst     : in    std_ulogic;
    clk     : in    std_ulogic;
    clken   : in    std_ulogic;

    csr     : inout csr_m_t;

    en      : in    std_ulogic;
    wop     : in    csr_wop_t;
    sel     : in    csra_t;
    wdata   : in    std_ulogic_vector;
    rdata   : out   std_ulogic_vector;
    nonex   : out   std_ulogic;

    instret : in    std_ulogic;

    mtime   : in    tval_t;

    excep   : in    std_ulogic;
    irq_e   : in    std_ulogic;
    irq_t   : in    std_ulogic;
    pc      : in    std_ulogic_vector;
    cause   : in    std_ulogic_vector;
    tval    : in    std_ulogic_vector;
    tinst   : in    std_ulogic_vector

  );
end entity csr_m;

architecture rtl of csr_m is

  subtype xval_t is std_ulogic_vector(isa.XLEN-1 downto 0);

  constant v0 : xval_t := (xval_t'range => '0');

  signal mip_hw : xval_t;

begin

  --------------------------------------------------------------------------------
  -- read only CSRs

  P_FIXED: process(all)
  begin

    csr.cycle      <= csr.mcycle;
    csr.time       <= mtime;
    csr.instret    <= csr.minstret;
    csr.mvendorid  <= v0;
    csr.marchid    <= v0;
    csr.mimpid     <= v0;
    csr.mhartid    <= v0;
    csr.mconfigptr <= v0;
    csr.misa       <= v0;
    csr.mtvec      <= mtvec(isa.XLEN-1 downto 2) & "00"; -- direct mode
    csr.mtval2     <= v0;

    csr.misa(31 downto 30) <= "10" when isa.XLEN = 64 else "01";
    csr.misa(12) <= bool2sl(isa.M);
    csr.misa( 8) <= bool2sl(isa.I);
    csr.misa( 5) <= bool2sl(isa.F);
    csr.misa( 4) <= bool2sl(isa.E);
    csr.misa( 3) <= bool2sl(isa.D);
    csr.misa( 2) <= bool2sl(isa.C);
    csr.misa( 0) <= bool2sl(isa.A);

  end process P_FIXED;

  --------------------------------------------------------------------------------

  P_MAIN: process(rst,clk)
    variable v_wdata : std_ulogic_vector(isa.XLEN-1 downto 0);
  begin
    if rst = '1' then

      csr.mstatus   <= (63 downto 0 => '0');
      csr.mie       <= v0;
      csr.mscratch  <= v0;
      csr.mepc      <= v0;
      csr.mcause    <= v0;
      csr.mtval     <= v0;
      csr.mip       <= v0;
      csr.mtinst    <= v0;
      csr.mcycle    <= (63 downto 0 => '0');
      csr.minstret  <= (63 downto 0 => '0');

    elsif rising_edge(clk) and clken = '1' then

      --------------------------------------------------------------------------------
      -- counters

      if unsigned(not csr.mcycle) = 0 then
        csr.mcycle <= v0;
      else
        csr.mcycle <= std_ulogic_vector(unsigned(csr.mcycle)+1);
      end if;

      if instret = '1' then
        if unsigned(not csr.minstret) = 0 then
          csr.minstret <= v0;
        else
          csr.minstret <= std_ulogic_vector(unsigned(csr.minstret)+1);
        end if;
      end if;

      --------------------------------------------------------------------------------
      -- exceptions

      if excep = '1' then
        csr.mepc   <= pc;
        csr.mcause <= cause;
        csr.mtval  <= tval;
        csr.mtinst <= tinst;
      end if;

      --------------------------------------------------------------------------------
      -- writes

      if en = '1' and wop /= CSR_WOP_NOP and nonex = '0' then
        case (wop) is
          when CSR_WOP_WR  => v_wdata := wdata;
          when CSR_WOP_SET => v_wdata := rdata or wdata;
          when CSR_WOP_CLR => v_wdata := rdata and not wdata;
          when others => null;
        end case;
        case (sel) is
          when CSRA_MSTATUS   => csr.mstatus  (isa.XLEN-1 downto 0) <= v_wdata;
          when CSRA_MIE       => csr.mie                            <= v_wdata;
          when CSRA_MSCRATCH  => csr.mscratch                       <= v_wdata;
          when CSRA_MEPC      => csr.mepc                           <= v_wdata;
          when CSRA_MCAUSE    => csr.mcause                         <= v_wdata;
          when CSRA_MTVAL     => csr.mtval                          <= v_wdata;
          when CSRA_MIP       => csr.mip                            <= v_wdata;
          when CSRA_MCYCLE    => csr.mcycle   (isa.XLEN-1 downto 0) <= v_wdata;
          when CSRA_MINSTRET  => csr.minstret (isa.XLEN-1 downto 0) <= v_wdata;
          when others =>
            if isa.XLEN = 32 then
              case (sel) is
                when CSRA_MSTATUSH  => csr.mstatus  (63 downto 32) <= v_wdata;
                when CSRA_MCYCLEH   => csr.mcycle   (63 downto 32) <= v_wdata;
                when CSRA_MINSTRETH => csr.minstret (63 downto 32) <= v_wdata;
                when others => null;
              end case;
            end if;
        end case;
      end if;

      --------------------------------------------------------------------------------
      -- fixed bit fields

      csr.mstatus( CSRB_MSTATUS_WPRI0 ) <= '0';
      csr.mstatus( CSRB_MSTATUS_SIE   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_WPRI2 ) <= '0';
      csr.mstatus( CSRB_MSTATUS_WPRI4 ) <= '0';
      csr.mstatus( CSRB_MSTATUS_SPIE  ) <= '0';
      csr.mstatus( CSRB_MSTATUS_UBE   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_MPIE  ) <= '1';
      csr.mstatus( CSRB_MSTATUS_SPP   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_VS0   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_VS1   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_MPP0  ) <= '1';
      csr.mstatus( CSRB_MSTATUS_MPP1  ) <= '1';
      csr.mstatus( CSRB_MSTATUS_FS0   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_FS1   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_XS0   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_XS1   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_MPRV  ) <= '0';
      csr.mstatus( CSRB_MSTATUS_SUM   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_MXR   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_TVM   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_TW    ) <= '0';
      csr.mstatus( CSRB_MSTATUS_TSR   ) <= '0';
      csr.mstatus( CSRB_MSTATUS_WPRI30 downto CSRB_MSTATUS_WPRI23) <= (others => '0');
      if isa.XLEN = 32 then
        csr.mstatus( 32+CSRB_MSTATUS_SD31 ) <= '0';
        csr.mstatus( 32+CSRB_MSTATUSH_WPRI3 downto CSRB_MSTATUSH_WPRI0) <= (others => '0');
        csr.mstatus( 32+CSRB_MSTATUSH_SBE ) <= '0';
        csr.mstatus( 32+CSRB_MSTATUSH_MBE ) <= '0';
        csr.mstatus( 32+CSRB_MSTATUSH_WPRI31 downto CSRB_MSTATUSH_WPRI6) <= (others => '0');
      elsif isa.XLEN = 64 then
        csr.mstatus( CSRB_MSTATUS_UXL0 ) <= csr.misa(isa.XLEN-2);
        csr.mstatus( CSRB_MSTATUS_UXL1 ) <= csr.misa(isa.XLEN-1);
        csr.mstatus( CSRB_MSTATUS_SXL0 ) <= csr.misa(isa.XLEN-2);
        csr.mstatus( CSRB_MSTATUS_SXL1 ) <= csr.misa(isa.XLEN-1);
        csr.mstatus( CSRB_MSTATUS_SBE  ) <= '0';
        csr.mstatus( CSRB_MSTATUS_MBE  ) <= '0';
        csr.mstatus( CSRB_MSTATUS_WPRI62 downto CSRB_MSTATUS_WPRI38) <= (others => '0');
        csr.mstatus( CSRB_MSTATUS_SD63 ) <= '0';
      end if;

      csr.mepc(0) <= '0';
      if not isa.C then
        csr.mepc(1) <= '0';
      end if;

      --------------------------------------------------------------------------------

    end if;
  end process P_MAIN;

  --------------------------------------------------------------------------------
  -- reads (and detection of accesses to non-existant CSRs)

  mip_hw <= (CSRB_MIP_MEIP => irq_e, CSRB_MIP_MTIP => irq_t, others => '0');

  P_READ: process(all)
  begin
    nonex <= '0';
    rdata <= (xval_t'range => 'X');
    case sel is
      when CSRA_CYCLE      => rdata <= csr.cycle      (isa.XLEN-1 downto 0) ; --  cycle counter for RDCYCLE instruction
      when CSRA_TIME       => rdata <= csr.time       (isa.XLEN-1 downto 0) ; --  timer for RDTIME instruction
      when CSRA_INSTRET    => rdata <= csr.instret    (isa.XLEN-1 downto 0) ; --  instructions-retired counter for RDINSTRET instruction
      when CSRA_MVENDORID  => rdata <= csr.mvendorid                        ; --  vendor ID
      when CSRA_MARCHID    => rdata <= csr.marchid                          ; --  architecture ID
      when CSRA_MIMPID     => rdata <= csr.mimpid                           ; --  implementation ID
      when CSRA_MHARTID    => rdata <= csr.mhartid                          ; --  hardware thread ID
      when CSRA_MCONFIGPTR => rdata <= csr.mconfigptr                       ; --  pointer to configuration data structure
      when CSRA_MSTATUS    => rdata <= csr.mstatus    (isa.XLEN-1 downto 0) ; --  machine status register
      when CSRA_MISA       => rdata <= csr.misa                             ; --  ISA and extensions
      when CSRA_MIE        => rdata <= csr.mie                              ; --  machine interrupt-enable register
      when CSRA_MTVEC      => rdata <= csr.mtvec                            ; --  machine trap-handler base address
      when CSRA_MSCRATCH   => rdata <= csr.mscratch                         ; --  scratch register for machine trap handlers
      when CSRA_MEPC       => rdata <= csr.mepc                             ; --  machine exception program counter
      when CSRA_MCAUSE     => rdata <= csr.mcause                           ; --  machine trap cause
      when CSRA_MTVAL      => rdata <= csr.mtval                            ; --  machine bad address or instruction
      when CSRA_MIP        => rdata <= csr.mip or mip_hw                    ; --  machine interrupt pending
      when CSRA_MTINST     => rdata <= csr.mtinst                           ; --  machine trap instruction (transformed)
      when CSRA_MTVAL2     => rdata <= csr.mtval2                           ; --  machine bad guest physical address
      when CSRA_MCYCLE     => rdata <= csr.mcycle     (isa.XLEN-1 downto 0) ; --  machine cycle counter
      when CSRA_MINSTRET   => rdata <= csr.minstret   (isa.XLEN-1 downto 0) ; --  machine instructions-retired counter
      when others =>
        if isa.XLEN = 32 then -- RV32 only: upper 32 bits of 64 bit CSRs
          case sel is
            when CSRA_CYCLEH     => rdata <= csr.cycle    (63 downto 32) ;
            when CSRA_TIMEH      => rdata <= csr.time     (63 downto 32) ;
            when CSRA_INSTRETH   => rdata <= csr.instret  (63 downto 32) ;
            when CSRA_MSTATUSH   => rdata <= csr.mstatus  (63 downto 32) ;
            when CSRA_MCYCLEH    => rdata <= csr.mcycle   (63 downto 32) ;
            when CSRA_MINSTRETH  => rdata <= csr.minstret (63 downto 32) ;
            when others          => nonex <= en;
          end case;
        else
          nonex <= en;
        end if;
    end case;
  end process P_READ;

end architecture rtl;
