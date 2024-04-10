-- tb_mcu3.vhd

use work.common_pkg.all;
use work.isa_pkg.all;
use work.mcu3_pkg.all;
use work.vc_if_pkg.all;
use work.vc_ls_pkg.all;
use work.vc_csr_pkg.all;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity tb_mcu3 is
  generic (
    isa_name      : string   := "RV32ICZicsr";
    mem_align     : boolean  := true;
    mem_size_log2 : positive range 4 to 32 := 12 -- 4kBytes default
  );
end entity tb_mcu3;

architecture sim of tb_mcu3 is

  constant isa : isa_t := isa_decode(isa_name);

  subtype xval_t is std_ulogic_vector(isa.XLEN-1 downto 0);
  type csr_t is array(0 to 4095) of xval_t;

  -- DUT ports
  signal rst       : std_ulogic;
  signal clk       : std_ulogic;
  signal clken     : std_ulogic;
  signal irq       : std_ulogic;
  signal mtime     : tval_t;
  signal mtimecmp  : tval_t;
  signal if_avalid : std_ulogic;
  signal if_ajmp   : std_ulogic;
  signal if_aaddr  : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
  signal if_amx    : std_ulogic;
  signal if_aready : std_ulogic;
  signal if_rvalid : std_ulogic;
  signal if_rdata  : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
  signal if_rready : std_ulogic;
  signal ls_avalid : std_ulogic;
  signal ls_aaddr  : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
  signal ls_amx    : std_ulogic;
  signal ls_aready : std_ulogic;
  signal ls_wvalid : std_ulogic;
  signal ls_wsize  : sz_t;
  signal ls_wdata  : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
  signal ls_wready : std_ulogic;
  signal ls_rvalid : std_ulogic;
  signal ls_rdata  : std_ulogic_vector(isa_decode(isa_name).XLEN-1 downto 0);
  signal ls_rready : std_ulogic;

  signal csr_x     : std_ulogic;
  signal csr_en    : std_ulogic;
  signal csr_wop   : csr_wop_t;
  signal csr_sel   : csra_t;
  signal csr_wdata : xval_t;
  signal csr_rdata : xval_t;

begin

  rst   <= '1', '0' after 20 ns;
  clk   <= '0' when clk = 'U' else not clk after 5 ns;
  clken <= '1';

  irq      <= '0';
  mtime    <= (others => '0');
  mtimecmp <= (others => '0');

  DUT: component mcu3
    generic map (
      isa_name => isa_name,
      rstvec   => x"00000000",
      mtvec    => x"00000010"
    )
    port map (
      rst       => rst,
      clk       => clk,
      clken     => clken,
      irq       => irq,
      mtime     => mtime,
      mtimecmp  => mtimecmp,
      if_avalid => if_avalid,
      if_ajmp   => if_ajmp,
      if_aaddr  => if_aaddr,
      if_amx    => if_amx,
      if_aready => if_aready,
      if_rvalid => if_rvalid,
      if_rdata  => if_rdata,
      if_rready => if_rready,
      ls_avalid => ls_avalid,
      ls_aaddr  => ls_aaddr,
      ls_amx    => ls_amx,
      ls_aready => ls_aready,
      ls_wvalid => ls_wvalid,
      ls_wsize  => ls_wsize,
      ls_wdata  => ls_wdata,
      ls_wready => ls_wready,
      ls_rvalid => ls_rvalid,
      ls_rdata  => ls_rdata,
      ls_rready => ls_rready,
      csr_x       => csr_x,
      csr_en      => csr_en,
      csr_wop     => csr_wop,
      csr_sel     => csr_sel,
      csr_wdata   => csr_wdata,
      csr_rdata_x => csr_rdata

    );

  U_VC_IF: component vc_if
    port map (
      rst    => rst,
      clk    => clk,
      clken  => clken,
      avalid => if_avalid,
      ajmp   => if_ajmp,
      aaddr  => if_aaddr,
      amx    => if_amx,
      aready => if_aready,
      rvalid => if_rvalid,
      rdata  => if_rdata,
      rready => if_rready
    );

  U_VC_LS: component vc_ls
    generic map (
      size_log2 => 12,
      align     => true
    )
    port map (
      rst    => rst,
      clk    => clk,
      clken  => clken,
      avalid => ls_avalid,
      aaddr  => ls_aaddr,
      amx    => ls_amx,
      aready => ls_aready,
      wvalid => ls_wvalid,
      wsize  => ls_wsize,
      wdata  => ls_wdata,
      wready => ls_wready,
      rvalid => ls_rvalid,
      rdata  => ls_rdata,
      rready => ls_rready
    );

  U_VC_CSR: component vc_csr
    port map (
      rst    => rst,
      clk    => clk,
      clken  => clken,
      x      => csr_x,
      en     => csr_en,
      wop    => csr_wop,
      sel    => csr_sel,
      wdata  => csr_wdata,
      rdata  => csr_rdata
    );

end architecture sim;