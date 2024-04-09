--------------------------------------------------------------------------------
-- alu.vhd                                                                    --
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

package alu_pkg is

  component alu is
    generic (
      isa   : isa_t
    );
    port (
      inst  : in    inst_t;
      pc    : in    std_ulogic_vector;
      rs1   : in    std_ulogic_vector;
      rs2   : in    std_ulogic_vector;
      rd    : out   std_ulogic_vector;
      eq    : out   std_ulogic;
      lt    : out   std_ulogic;
      ltu   : out   std_ulogic
    );
  end component alu;

end package alu_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;
use work.isa_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity alu is
  generic (
    isa   : isa_t
  );
  port (
    inst  : in    inst_t;
    pc    : in    std_ulogic_vector;
    rs1   : in    std_ulogic_vector;
    rs2   : in    std_ulogic_vector;
    rd    : out   std_ulogic_vector;
    eq    : out   std_ulogic;
    lt    : out   std_ulogic;
    ltu   : out   std_ulogic
  );
end entity alu;

architecture rtl of alu is

  subtype xval_t is std_ulogic_vector(isa.XLEN-1 downto 0);

  constant v0 : xval_t := (others => '0');
  constant v1 : xval_t := (0 => '1', others => '0');

  signal opvec : opvec_t;
  signal c     : std_ulogic;
  signal f3    : f3_t;
  signal f7    : f7_t;
  signal imm12 : imm12_t;
  signal imm20 : imm20_t;

  function v(s : signed) return xval_t is
  begin
    return xval_t(s);
  end function v;

  function v(u : unsigned) return xval_t is
  begin
    return xval_t(u);
  end function v;

  function s(v : xval_t) return signed is
  begin
    return signed(v);
  end function s;

  function u(v : xval_t) return unsigned is
  begin
    return unsigned(v);
  end function u;

  function r(s : signed) return signed is
  begin
    return resize(s,isa.XLEN);
  end function r;

  function r(u : unsigned) return unsigned is
  begin
    return resize(u,isa.XLEN);
  end function r;

  function i(v : std_ulogic_vector) return integer is
  begin
    return to_integer(unsigned(v));
  end function i;

begin

  P_MAIN: process(all)
  begin
    opvec <= inst(6 downto 2);
    c     <= inst(1) nand inst(0);
    f3    <= inst(14 downto 12);
    f7    <= inst(31 downto 25);
    imm12 <= inst(31 downto 20);
    imm20 <= inst(31 downto 12);
    rd    <= (rd'range => '-');
    if opvec(2) = '0' then -- JAL, JALR, BRANCH
      rd <= xval_t(unsigned(pc)+2) when c = '1' else xval_t(unsigned(pc)+4);
    else
      if opvec(0) = '1' and opvec(3) = '1' then -- LUI
        rd <= imm20 & x"000";
      elsif opvec(0) = '1' and opvec(3) = '0' then -- AUIPC
        rd <= v(u(pc) + u(imm20 & x"000"));
      elsif opvec(0) = '0' and opvec(3) = '1' then -- OP
        case f3 is
          when F3_ADDSUB =>
            case f7(5) is
              when '0'    => rd <= v(u(rs1) + u(rs2));
              when '1'    => rd <= v(u(rs1) - u(rs2));
              when others => rd <= (rd'range => 'X');
            end case;
          when F3_SLT  => rd <= v1 when lt  = '1' else v0;
          when F3_SLTU => rd <= v1 when ltu = '1' else v0;
          when F3_SLL  => rd <= v(shift_left(u(rs1),i(rs2(4 downto 0))));
          when F3_SR =>
            case f7(5) is
              when '0'    => rd <= v(shift_right(u(rs1),i(rs2(4 downto 0))));
              when '1'    => rd <= v(shift_right(s(rs1),i(rs2(4 downto 0))));
              when others => rd <= (rd'range => 'X');
            end case;
          when F3_AND  => rd <= rs1 and rs2;
          when F3_OR   => rd <= rs1 or  rs2;
          when F3_XOR  => rd <= rs1 xor rs2;
          when others  => rd <= (rd'range => 'X');
        end case;
      elsif opvec(0) = '0' and opvec(3) = '0' then -- OP-IMM
        case f3 is
          when F3_ADDI  => rd <= v(s(rs1) + r(s(imm12)));
          when F3_SLTI  => rd <= v1 when s(rs1) < r(s(imm12)) else v0;
          when F3_SLTIU => rd <= v1 when u(rs1) < r(u(imm12)) else v0;
          when F3_SLLI  => rd <= v(shift_left(u(rs1),i(imm12(4 downto 0))));
          when F3_SRI =>
            case f7(5) is
              when '0'    => rd <= v(shift_right(u(rs1),i(imm12(4 downto 0))));
              when '1'    => rd <= v(shift_right(s(rs1),i(imm12(4 downto 0))));
              when others => rd <= (rd'range => 'X');
            end case;
          when F3_ANDI  => rd <= v(s(rs1) and r(s(imm12)));
          when F3_ORI   => rd <= v(s(rs1) or  r(s(imm12)));
          when F3_XORI  => rd <= v(s(rs1) xor r(s(imm12)));
          when others   => rd <= (rd'range => 'X');
        end case;
      end if;
    end if;
  end process P_MAIN;

  eq  <= bool2sl(rs1 = rs2);
  lt  <= bool2sl(s(rs1) < s(rs2));
  ltu <= bool2sl(u(rs1) < u(rs2));

end architecture rtl;
