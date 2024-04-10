--------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;

package vc_if_pkg is

  component vc_if is
    port (
      rst    : in    std_ulogic;
      clk    : in    std_ulogic;
      clken  : in    std_ulogic;
      avalid : in    std_ulogic;
      ajmp   : in    std_ulogic;
      aaddr  : in    std_ulogic_vector;
      amx    : out   std_ulogic;
      aready : out   std_ulogic;
      rvalid : out   std_ulogic;
      rdata  : out   std_ulogic_vector(31 downto 0);
      rready : in    std_ulogic
    );
  end component vc_if;

end package vc_if_pkg;

--------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;

entity vc_if is
  port (

    rst     : in    std_ulogic;
    clk     : in    std_ulogic;
    clken   : in    std_ulogic;

    -- instruction fetch address channel
    avalid : in    std_ulogic;                     -- instruction address valid
    ajmp   : in    std_ulogic;                     -- reset/exception/jump/branch
    aaddr  : in    std_ulogic_vector;              -- instruction address
    amx    : out   std_ulogic;                     -- load/store misalign exceptions
    aready : out   std_ulogic;                     -- instruction address ready

    -- instruction fetch read data channel
    rvalid : out   std_ulogic;                     -- instruction data valid
    rdata  : out   std_ulogic_vector(31 downto 0); -- instruction data
    rready : in    std_ulogic                      -- instruction data ready

  );
end entity vc_if;

architecture sim of vc_if is
begin

  -- instruction fetches always return NOPs
  aready <= '1';
  amx    <= '0';
  process(rst,clk)
  begin
    if rst = '1' then
      rvalid <= '0';
      rdata  <= (others => 'X');
    elsif rising_edge(clk) and clken = '1' then
      rvalid <= (avalid and aready) or (rvalid and not rready);
      rdata  <= X"00000013" when avalid and aready else (others => 'X') when rvalid and not rready;
    end if;
  end process;

end architecture sim;
