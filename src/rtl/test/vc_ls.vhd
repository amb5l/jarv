--------------------------------------------------------------------------------

use work.common_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;

package vc_ls_pkg is

  component vc_ls is
      generic (
      size_log2 : natural range 4 to 32;
      align     : boolean
    );
    port (
      rst    : in    std_ulogic;
      clk    : in    std_ulogic;
      clken  : in    std_ulogic;
      avalid : in    std_ulogic;
      aaddr  : in    std_ulogic_vector;
      amx    : out   std_ulogic;
      aready : out   std_ulogic;
      wvalid : in    std_ulogic;
      wsize  : in    sz_t;
      wdata  : in    std_ulogic_vector;
      wready : out   std_ulogic;
      rvalid : out   std_ulogic;
      rdata  : out   std_ulogic_vector;
      rready : in    std_ulogic
    );
  end component vc_ls;

end package vc_ls_pkg;

--------------------------------------------------------------------------------

use work.common_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity vc_ls is
  generic (
    size_log2 : natural range 4 to 32;
    align     : boolean
  );
  port (

    rst    : in    std_ulogic;
    clk    : in    std_ulogic;
    clken  : in    std_ulogic;

    -- load/store address channel
    avalid : in    std_ulogic;        -- load/store address valid
    aaddr  : in    std_ulogic_vector; -- load/store address
    amx    : out   std_ulogic;        -- load/store misalign exceptions
    aready : out   std_ulogic;        -- load/store address ready

    -- load/store write data channel
    wvalid : in    std_ulogic;        -- load/store write data valid
    wsize  : in    sz_t;              -- load/store size
    wdata  : in    std_ulogic_vector; -- load/store write data
    wready : out   std_ulogic;        -- load/store write data ready

    -- load/store read data channel
    rvalid : out   std_ulogic;        -- load/store read data valid
    rdata  : out   std_ulogic_vector; -- load/store read data
    rready : in    std_ulogic         -- load/store read data ready

  );
end entity vc_ls;

architecture sim of vc_ls is

  constant XLEN : natural := aaddr'length;

  subtype xval_t is std_ulogic_vector(XLEN-1 downto 0);
  type mem_t is array(0 to (2**(size_log2-((XLEN/32)+1)))-1) of xval_t;

  signal mem : mem_t;

begin

  aready <= '1';
  wready <= '1';
  amx    <= '0';
  P_MEM: process(clk)
    variable v_addr : integer range 0 to (2**(size_log2-((XLEN/32)+1)))-1;
  begin
    assert align
      report "unaligned memory access not yet supported" severity failure;
    if rst = '1' then
      rvalid <= '0';
      rdata  <= (rdata'range => 'X');
    elsif rising_edge(clk) and clken = '1' then
      if rvalid and rready then
        rvalid <= '0';
        rdata  <= (rdata'range => 'X');
      end if;
      if avalid and aready then
        v_addr := to_integer(unsigned(aaddr(size_log2-1 downto 2)));
        rvalid <= '1';
        rdata  <= mem(v_addr);
      end if;
      if wvalid and wready then
        mem(v_addr)(7 downto 0) <= wdata(7 downto  0);
        if wsize /= SZ_B then
          mem(v_addr)(15 downto 8) <= wdata(15 downto 8);
        end if;
        if wsize = SZ_W or wsize = SZ_D then
          mem(v_addr)(31 downto 16) <= wdata(31 downto 16);
        end if;
        if XLEN = 64 and wsize = SZ_D then
          mem(v_addr)(63 downto 32) <= wdata(63 downto 32);
        end if;
      end if;
    end if;
  end process P_MEM;

end architecture sim;
