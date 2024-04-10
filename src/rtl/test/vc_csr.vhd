--------------------------------------------------------------------------------

use work.common_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;

package vc_csr_pkg is

  component vc_csr is
    port (
      rst   : in    std_ulogic;
      clk   : in    std_ulogic;
      clken : in    std_ulogic;
      x     : out   std_ulogic := '0';
      en    : in    std_ulogic;
      wop   : in    csr_wop_t;
      sel   : in    csra_t;
      wdata : in    std_ulogic_vector;
      rdata : out   std_ulogic_vector
    );
  end component vc_csr;

end package vc_csr_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity vc_csr is
  port (

    rst   : in    std_ulogic;
    clk   : in    std_ulogic;
    clken : in    std_ulogic;

    x     : out   std_ulogic := '0';
    en    : in    std_ulogic;
    wop   : in    csr_wop_t;
    sel   : in    csra_t;
    wdata : in    std_ulogic_vector;
    rdata : out   std_ulogic_vector

  );
end entity vc_csr;

architecture sim of vc_csr is

  constant XLEN : natural := wdata'length;

  subtype xval_t is std_ulogic_vector(XLEN-1 downto 0);
  type csr_t is array(0 to 4095) of xval_t;

  signal csr : csr_t;

begin

  x <= '1';

  P_CSR: process(all)
  begin
    if rst = '1' then
      csr <= (others => (others => '0'));
    elsif rising_edge(clk) and clken = '1' then
      if en = '1' then
        if wop = CSR_WOP_WR then
          csr(to_integer(unsigned(sel))) <= wdata;
        elsif wop = CSR_WOP_SET then
          csr(to_integer(unsigned(sel))) <= rdata or wdata;
        elsif wop = CSR_WOP_CLR then
          csr(to_integer(unsigned(sel))) <= rdata and not wdata;
        end if;
      end if;
    end if;
    rdata <= csr(to_integer(unsigned(sel)));
  end process P_CSR;

end architecture sim;
