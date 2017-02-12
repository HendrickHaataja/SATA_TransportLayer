library ieee;                   --! Use standard library.
use ieee.std_logic_1164.all;    --! Use standard logic elements
use ieee.numeric_std.all;       --! Use numeric standard

entity transport_layer_tb is
end transport_layer_tb;

architecture behavior of transport_layer_tb is

--user interface
signal clk : std_logic := '0';
signal rst_n : std_logic := '1';
signal user_data_in, user_data_out : std_logic_vector(31 downto 0);
signal user_addr_in, user_addr_out : std_logic_vector(31 downto 0);
signal status : std_logic_vector(3 downto 0);
signal command : std_logic_vector(2 downto 0);

--link interface
signal link_status	: std_logic_vector(31 downto 0);
signal data_to_link, data_from_link	: std_logic_vector(31 downto 0);

constant clk_period : time := 1 ns; --not accurate to device

type fake_sata_memory_type is array (31 downto 0) of std_logic_vector(31 downto 0);
signal fake_memory : fake_sata_memory_type;

--component for DUT--
component transport_layer 	
   port(
		--Interface with Application Layer
		rst_n			:	in std_logic;
		clk			:	in std_logic;
		
		write_data		:	in std_logic_vector(31 downto 0);
		write_address	:	in std_logic_vector(31 downto 0);
		
		user_command			:	in std_logic_vector(2 downto 0);
		status_to_user			:	out std_logic_vector(3 downto 0); 	
		
		read_data		:	out std_logic_vector(31 downto 0);
		read_address	:	out std_logic_vector(31 downto 0);
		
		--Interface with Link Layer
		link_status		:	in std_logic_vector(31 downto 0);
		tx_data_out		:	out std_logic_vector(31 downto 0);
		rx_data_in		:	in std_logic_vector(31 downto 0));

end component;



begin

	dut: transport_layer port map(rst_n => rst_n, clk => clk,
								  write_data => user_data_in,
								  write_address => user_addr_in,
								  user_command => command,
								  status_to_user => status,
								  read_data => user_data_out,
								  read_address => user_addr_out,
								  link_status => link_status,
								  tx_data_out => data_to_link,
								  rx_data_in => data_from_link);

	clk_process	: process
	begin	
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
	end process;
	
	stim_proc : process
	  begin
		wait for 2 ns;
		rst_n <= '0';
		wait for 2 ns;
		rst_n <= '1';
		wait for 2 ns;
		command <= "001";


		for i in 0 to 31 loop
			user_data_in <= std_logic_vector(to_unsigned(i, 32));
			wait until rising_edge(clk);
		end loop;
		wait until rising_edge(clk);
	
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		wait until rising_edge(clk);

		command <= "000";

		wait until rising_edge(clk);
		wait until rising_edge(clk);
		for j in 0 to 31 loop
			fake_memory(j) <= data_to_link;
			wait until rising_edge(clk);
		end loop;



		command <= "010";

		wait until rising_edge(clk);
		wait until rising_edge(clk);
		for k in 0 to 31 loop
			data_from_link <= fake_memory(k);
			wait until rising_edge(clk);
		end loop;

		wait until rising_edge(clk);
		wait until rising_edge(clk);
		wait until rising_edge(clk);

		command <= "100";
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		while status(3) = '1' loop
			wait until rising_edge(clk);
		end loop;
		
	end process;

end architecture;