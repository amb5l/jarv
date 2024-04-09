--------------------------------------------------------------------------------
-- decoder.vhd                                                                --
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

package decoder_pkg is

  type d_t is record
    legal   : std_ulogic;
    opcode  : opcode_t;
    mnemonic: mnemonic_t;
    f3      : f3_t;
    f7      : f7_t;
    imm12   : imm12_t;
    imm20   : imm20_t;
    imm12b  : imm12_t;
    rs1_en  : std_ulogic;
    rs1_sel : xsel_t;
    rs2_en  : std_ulogic;
    rs2_sel : xsel_t;
    rd_en   : std_ulogic;
    rd_sz   : ssz_t;
    rd_sel  : xsel_t;
    csr_en  : std_ulogic;
    csr_r   : std_ulogic;
    csr_wop : csr_wop_t;
    csr_sel : csra_t;
  end record d_t;

  component decoder is
    generic (
      isa   : isa_t
    );
    port (
      valid : in    std_ulogic;
      i     : in    inst_t;
      o     : out   inst_t;
      d     : out   d_t
    );
  end component decoder;

end package decoder_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;
use work.isa_pkg.all;
use work.decoder_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity decoder is
  generic (
    isa   : isa_t
  );
  port (
    valid : in    std_ulogic;
    i     : in    inst_t;
    o     : out   inst_t;
    d     : out   d_t
  );
end entity decoder;

architecture rtl of decoder is

  constant iset : inst_set_t := inst_set(isa);

begin

  P_DECODE: process(all)
    variable idef : inst_def_t;
  begin
    o          <= uncompress(i,isa) when isa.C else i;
    d.legal    <= '0';
    d.opcode   <= opcode_t'val(to_integer(unsigned(o(6 downto 2))));
    d.mnemonic <= U_UNKNOWN;
    d.f3       <= o(14 downto 12);
    d.f7       <= o(31 downto 25);
    d.imm12    <= o(31 downto 20);
    d.imm20    <= o(31 downto 12);
    d.imm12b   <= o(31) & o(7) & o(30 downto 25) & o(11 downto 8);
    d.rs1_en   <= '0';
    d.rs1_sel  <= o(19 downto 15);
    d.rs2_en   <= '0';
    d.rs2_sel  <= o(24 downto 20);
    d.rd_en    <= '0';
    d.rd_sz    <= F3_SZ_W;
    d.rd_sel   <= o(11 downto  7);
    d.csr_en   <= '0';
    d.csr_r    <= '0';
    d.csr_wop  <= CSR_WOP_NOP;
    d.csr_sel <= (others => '0');
    if valid = '1' then
      for i in 0 to iset'length-1 loop
        idef := iset(i);
        if pattern_match(o,idef.pattern) then
          d.legal    <= '1';
          d.mnemonic <= idef.mnemonic;
          d.rs1_en   <= idef.rs1;
          d.rs2_en   <= idef.rs2;
          d.rd_en    <= idef.rd;
          d.rd_sz    <= d.f3 when d.opcode = LOAD else F3_SZ_D when isa.XLEN = 64 else F3_SZ_W;
          if isa.Zicsr then
            d.csr_en   <= '1' when d.opcode = SYSTEM and unsigned(d.f3) /= 0 else '0';
            d.csr_r    <= '1' when unsigned(d.rd_sel) /= 0 else '0';
            d.csr_wop  <=
              CSR_WOP_WR  when (d.mnemonic = U_CSRRW or d.mnemonic = U_CSRRWI) else
              CSR_WOP_SET when (d.mnemonic = U_CSRRS or d.mnemonic = U_CSRRSI) else
              CSR_WOP_CLR when (d.mnemonic = U_CSRRC or d.mnemonic = U_CSRRCI) else
              CSR_WOP_NOP;
            d.csr_sel <= o(31 downto 20);
          end if;
          exit;
        end if;
      end loop;
    end if;
  end process P_DECODE;


end architecture rtl;
