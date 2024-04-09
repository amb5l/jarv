--------------------------------------------------------------------------------
-- test_fit_mcu3.vhd                                                          --
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

use work.mcu3_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity fpga is
  generic (
    isa_name   : string := "RV32I";
    rstvec     : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0) := (others => '0');
    mtvec      : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0) := (4 => '1', others => '0')
  );
  port (
    rst       : in    std_ulogic;
    clk       : in    std_ulogic;
    clken     : in    std_ulogic;
    irq       : in    std_ulogic;
    if_avalid : out   std_ulogic;
    if_ajmp   : out   std_ulogic;
    if_aaddr  : out   std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
    if_amx    : in    std_ulogic;
    if_aready : in    std_ulogic;
    if_rvalid : in    std_ulogic;
    if_rdata  : in    std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
    if_rready : out   std_ulogic;
    ls_avalid : out   std_ulogic;
    ls_aaddr  : out   std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
    ls_amx    : in    std_ulogic;
    ls_aready : in    std_ulogic;
    ls_wvalid : out   std_ulogic;
    ls_wsize  : out   sz_t;
    ls_wdata  : out   std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
    ls_wready : in    std_ulogic;
    ls_rvalid : in    std_ulogic;
    ls_rdata  : in    std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
    ls_rready : out   std_ulogic
  );
end entity fpga;

architecture rtl of fpga is

  signal mtime    : tval_t;
  signal mtimecmp : tval_t;

begin

  mtime    <= (others => '0');
  mtimecmp <= (others => '0');

  CORE: component mcu3
    generic map (
      isa_name   => isa_name,
      rstvec     => rstvec,
      mtvec      => mtvec
    )
    port map (
      rst       => rst       ,
      clk       => clk       ,
      clken     => clken     ,
      irq       => irq       ,
      mtime     => mtime     ,
      mtimecmp  => mtimecmp  ,
      if_avalid => if_avalid ,
      if_ajmp   => if_ajmp   ,
      if_aaddr  => if_aaddr  ,
      if_amx    => if_amx    ,
      if_aready => if_aready ,
      if_rvalid => if_rvalid ,
      if_rdata  => if_rdata  ,
      if_rready => if_rready ,
      ls_avalid => ls_avalid ,
      ls_aaddr  => ls_aaddr  ,
      ls_amx    => ls_amx    ,
      ls_aready => ls_aready ,
      ls_wvalid => ls_wvalid ,
      ls_wsize  => ls_wsize  ,
      ls_wdata  => ls_wdata  ,
      ls_wready => ls_wready ,
      ls_rvalid => ls_rvalid ,
      ls_rdata  => ls_rdata  ,
      ls_rready => ls_rready
    );

end architecture rtl;
