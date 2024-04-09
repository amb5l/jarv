-- tb_mcu3.vhd

use work.common_pkg.all;
use work.isa_pkg.all;
use work.mcu3_pkg.all;

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
  type mem_t is array(0 to (2**(3+mem_size_log2-isa.XLEN))-1) of xval_t;

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

  signal mem       : mem_t;

  signal csr       : csr_t;
  signal csr_rdata : xval_t;

  -- DUT external access
  alias dut_csr_en    is << signal DUT.csr_en    : std_ulogic >>;
  alias dut_csr_wop   is << signal DUT.csr_wop   : csr_wop_t  >>;
  alias dut_csr_sel   is << signal DUT.csr_sel   : csra_t     >>;
  alias dut_csr_wdata is << signal DUT.csr_wdata : xval_t     >>;
  alias dut_csr_rdata is << signal DUT.csr_rdata : xval_t     >>;

begin

  rst   <= '1', '0' after 100 ns;
  clk   <= '0' when clk = 'U' else not clk after 5 ns;
  clken <= '1';

  --------------------------------------------------------------------------------

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
      ls_rready => ls_rready
    );

  --------------------------------------------------------------------------------

  -- instruction fetches always return NOPs
  if_aready <= '1';
  if_amx    <= '0';
  process(rst,clk)
  begin
    if rst = '1' then
      if_rvalid <= '0';
      if_rdata  <= (others => 'X');
    elsif rising_edge(clk) then
      if_rvalid <= (if_avalid and if_aready) or (if_rvalid and not if_rready);
      if_rdata  <= X"00000013" when if_avalid and if_aready else (others => 'X') when if_rvalid and not if_rready;
    end if;
  end process;

  --------------------------------------------------------------------------------
  -- dummy CSR register file

  csr_rdata <= csr(to_integer(unsigned(dut_csr_sel)));
  P_CSR: process(rst,clk)
  begin
    if rst = '1' then
      csr <= (others => (others => '0'));
    elsif rising_edge(clk) then
      if dut_csr_en = '1' then
        if dut_csr_wop = CSR_WOP_WR then
          csr(to_integer(unsigned(dut_csr_sel))) <= dut_csr_wdata;
        elsif dut_csr_wop = CSR_WOP_SET then
          csr(to_integer(unsigned(dut_csr_sel))) <= dut_csr_rdata or dut_csr_wdata;
        elsif dut_csr_wop = CSR_WOP_CLR then
          csr(to_integer(unsigned(dut_csr_sel))) <= dut_csr_rdata and not dut_csr_wdata;
        end if;
      end if;
    end if;
  end process P_CSR;

  --------------------------------------------------------------------------------
  -- data memory
  -- need to improve this to decouple address and write data

  ls_aready <= '1';
  ls_wready <= '1';
  ls_amx    <= '0';
  P_MEM: process(clk)
    variable v_addr : integer range 0 to 2**(3+mem_size_log2-isa.XLEN)-1;
  begin
    assert mem_align
      report "unaligned memory access not yet supported" severity failure;
    if rst = '1' then
      ls_rvalid <= '0';
      ls_rdata  <= (others => 'X');
    elsif rising_edge(clk) then
      if ls_rvalid and ls_rready then
        ls_rvalid <= '0';
        ls_rdata  <= (others => 'X');
      end if;
      if ls_avalid and ls_aready then
        v_addr := to_integer(unsigned(ls_aaddr(mem_size_log2-1 downto 2)));
        ls_rvalid <= '1';
        ls_rdata  <= mem(v_addr);
      end if;
      if ls_wvalid and ls_wready then
        mem(v_addr)(7 downto 0) <= ls_wdata(7 downto  0);
        if ls_wsize /= SZ_B then
          mem(v_addr)(15 downto 8) <= ls_wdata(15 downto 8);
        end if;
        if ls_wsize = SZ_W or ls_wsize = SZ_D then
          mem(v_addr)(31 downto 16) <= ls_wdata(31 downto 16);
        end if;
        if isa.XLEN = 64 and ls_wsize = SZ_D then
          mem(v_addr)(63 downto 32) <= ls_wdata(63 downto 32);
        end if;
      end if;
    end if;
  end process P_MEM;

  --------------------------------------------------------------------------------
  -- inject stimulus instructions and dummy CSR read data into DUT

  INJECT: process(all)
  begin
    dut_csr_rdata <= force csr_rdata;
  end process INJECT;

  --------------------------------------------------------------------------------

end architecture sim;