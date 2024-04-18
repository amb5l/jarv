--------------------------------------------------------------------------------
-- regfile_d.vhd                                                              --
-- JARV register file with delayed/concurrent write support.                  --
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
-- Should infer simple dual port RAM primitives with asynchronous read and
-- synchronous write ports.
-- RAM required is 4x the number of registers because:
-- 1) we need to read 2 source registers in every cycle;
-- 2) we need to handle dual concurrent writes (immediate and delayed).
-- Concurrent writes are handled by pingponging between alternate register
-- sets (a and b). In case of collision, rd takes precedence over ld.
-- When reading from a source register that is the target of a delayed write,
-- the delayed write value is used in preference to the current register file
-- contents.

use work.common_pkg.all;
use work.isa_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;

package regfile_d_pkg is

  component regfile_d is
    generic (
      isa      : isa_t;
      rs_sync  : boolean := false
    );
    port (
      rst      : in    std_ulogic;
      clk      : in    std_ulogic;
      clken    : in    std_ulogic;
      rd_en    : in    std_ulogic;
      rd_ld    : in    std_ulogic;
      rd_sz    : in    ssz_t;
      rd_sel   : in    xsel_t;
      rd_data  : in    std_ulogic_vector;
      ld_data  : in    std_ulogic_vector;
      ld_rdy   : in    std_ulogic;
      rs1_sel  : in    xsel_t;
      rs1_data : out   std_ulogic_vector;
      rs2_sel  : in    xsel_t;
      rs2_data : out   std_ulogic_vector
    );
  end component regfile_d;

end package regfile_d_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;
use work.isa_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity regfile_d is
  generic (
    isa      : isa_t;
    rs_sync  : boolean := false
  );
  port (
    rst      : in    std_ulogic;        -- reset
    clk      : in    std_ulogic;        -- clock
    clken    : in    std_ulogic;        -- clock enable
    rd_en    : in    std_ulogic;        -- destination register enable
    rd_ld    : in    std_ulogic;        -- destination register data is from load (is delayed)
    rd_sz    : in    ssz_t;             -- destination register size (for delayed writes: B, H, L, D, BU, HU, WU)
    rd_sel   : in    xsel_t;            -- destination register select
    rd_data  : in    std_ulogic_vector; -- destination register data
    ld_data  : in    std_ulogic_vector; -- load destination register data
    ld_rdy   : in    std_ulogic;        -- load destination register ready
    rs1_sel  : in    xsel_t;            -- source register 1 select
    rs1_data : out   std_ulogic_vector; -- source register 1 data
    rs2_sel  : in    xsel_t;            -- source register 2 select
    rs2_data : out   std_ulogic_vector  -- source register 2 data
  );
end entity regfile_d;

architecture rtl of regfile_d is

  subtype xval_t is std_ulogic_vector(isa.XLEN-1 downto 0);

  type regfile_t is array(0 to 31) of xval_t;

  signal ab        : std_ulogic_vector(0 to 31);
  signal rd_ab     : std_logic;
  signal rd_we     : std_ulogic;
  signal ld_ab     : std_logic;
  signal ld_we     : std_ulogic;
  signal ld_sz     : ssz_t;
  signal ld_sel    : xsel_t;
  signal ld_sdata  : xval_t;
  signal regfile1a : regfile_t := (others => (others => '0'));
  signal regfile2a : regfile_t := (others => (others => '0'));
  signal regfile1b : regfile_t := (others => (others => '0'));
  signal regfile2b : regfile_t := (others => (others => '0'));
  signal rs1_data_a : xval_t;                                  -- asynchronous
  signal rs2_data_a : xval_t;                                  -- asynchronous
  signal rs1_data_s : xval_t;                                  -- synchronous
  signal rs2_data_s : xval_t;                                  -- synchronous

begin

  rd_ab <= ab(to_integer(unsigned(rd_sel)));
  rd_we <= rd_en and not rd_ld;

  ld_ab <= ab(to_integer(unsigned(ld_sel)));
  ld_we <= ld_rdy;
  ld_sdata(7 downto 0) <= ld_data(7 downto 0);
  ld_sdata(15 downto 8) <=
    ld_data(15 downto 8) when ld_sz(1 downto 0) /= "00" else
    (others => '0') when ld_sz(2) = '1' else
    (others => ld_data(7));
  ld_sdata(31 downto 16) <=
    ld_data(31 downto 16) when ld_sz(1) = '1' else
    (others => '0') when ld_sz(2) = '1' else
    (others => ld_data(15)) when ld_sz(0) = '1' else
    (others => ld_data(7));

  P_WRITE: process(rst,clk)
  begin
    if rst = '1' then
      ab     <= (others => '0');
      ld_sz  <= (others => '0');
      ld_sel <= (others => '0');
    elsif rising_edge(clk) and clken = '1' then
      if rd_en = '1' then
        ld_sz  <= rd_sz;
        ld_sel <= rd_sel;
      end if;
      -- toggle delayed write ab when both writes hit the same bank
      if rd_we = '1' and ld_we = '1' and rd_sel /= ld_sel and rd_ab = ld_ab then
        ab(to_integer(unsigned(ld_sel))) <= not ld_ab;
      end if;
      if rd_we and ld_we then
        if rd_ab = '0' then
          regfile1a(to_integer(unsigned(rd_sel))) <= rd_data;
          regfile2a(to_integer(unsigned(rd_sel))) <= rd_data;
          regfile1b(to_integer(unsigned(ld_sel))) <= ld_sdata;
          regfile2b(to_integer(unsigned(ld_sel))) <= ld_sdata;
        else
          regfile1b(to_integer(unsigned(rd_sel))) <= rd_data;
          regfile2b(to_integer(unsigned(rd_sel))) <= rd_data;
          regfile1a(to_integer(unsigned(ld_sel))) <= ld_sdata;
          regfile2a(to_integer(unsigned(ld_sel))) <= ld_sdata;
        end if;
        if rd_sel /= ld_sel and rd_ab = ld_ab then
          ab(to_integer(unsigned(ld_sel))) <= not ld_ab;
        end if;
      elsif rd_we then
        if rd_ab = '0' then
          regfile1a(to_integer(unsigned(rd_sel))) <= rd_data;
          regfile2a(to_integer(unsigned(rd_sel))) <= rd_data;
        else
          regfile1b(to_integer(unsigned(rd_sel))) <= rd_data;
          regfile2b(to_integer(unsigned(rd_sel))) <= rd_data;
        end if;
      elsif ld_we then
        if ld_ab = '0' then
          regfile1a(to_integer(unsigned(ld_sel))) <= ld_sdata;
          regfile2a(to_integer(unsigned(ld_sel))) <= ld_sdata;
        else
          regfile1b(to_integer(unsigned(ld_sel))) <= ld_sdata;
          regfile2b(to_integer(unsigned(ld_sel))) <= ld_sdata;
        end if;
      end if;
    end if;
  end process;

  P_READ1: process(all)
  begin
    if unsigned(rs1_sel) = 0 then
      rs1_data_a <= (rs1_data'range => '0');
    elsif ld_we = '1' and ld_sel = rs1_sel then
      rs1_data_a <= ld_sdata;
    elsif ab(to_integer(unsigned(rs1_sel))) = '1' then
      rs1_data_a <= regfile1b(to_integer(unsigned(rs1_sel)));
    else
      rs1_data_a <= regfile1a(to_integer(unsigned(rs1_sel)));
    end if;
  end process P_READ1;

  P_READ2: process(all)
  begin
    if unsigned(rs2_sel) = 0 then
      rs2_data_a <= (rs2_data'range => '0');
    elsif ld_we = '1' and ld_sel = rs2_sel then
      rs2_data_a <= ld_sdata;
    elsif ab(to_integer(unsigned(rs2_sel))) = '1' then
      rs2_data_a <= regfile2b(to_integer(unsigned(rs2_sel)));
    else
      rs2_data_a <= regfile2a(to_integer(unsigned(rs2_sel)));
    end if;
  end process P_READ2;

  P_READ_SYNC: process(rst,clk)
  begin
    if rst = '1' then
      rs1_data_s <= (others => '0');
      rs2_data_s <= (others => '0');
    elsif rising_edge(clk) and clken = '1' then
      rs1_data_s <= rs1_data_a;
      rs2_data_s <= rs2_data_a;
    end if;
  end process P_READ_SYNC;

  rs1_data <= rs1_data_s when rs_sync else rs1_data_a;
  rs2_data <= rs2_data_s when rs_sync else rs2_data_a;

end architecture rtl;
