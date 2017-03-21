library ieee;                   --! Use standard library.
use ieee.std_logic_1164.all;    --! Use standard logic elements
use ieee.numeric_std.all;       --! Use numeric standard

entity t_layer_fsm_32_tb is
end t_layer_fsm_32_tb;

architecture behavior of t_layer_fsm_32_tb is

--user interface
signal clk : std_logic := '0';
signal rst_n : std_logic := '1';
signal user_data_in, user_data_out : std_logic_vector(31 downto 0);
signal user_addr_in, user_addr_out : std_logic_vector(31 downto 0);
signal status : std_logic_vector(3 downto 0);
signal command : std_logic_vector(2 downto 0);

--link interface
signal transport_status : std_logic_vector(7 downto 0);
signal link_status	: std_logic_vector(31 downto 0);
signal data_to_link, data_from_link	: std_logic_vector(31 downto 0);

constant clk_period : time := 1 ns; --not accurate to device

type fake_sata_memory_type is array (31 downto 0) of std_logic_vector(31 downto 0);
signal fake_memory : fake_sata_memory_type;

signal sending_data : std_logic := '0';

--component for DUT--
component t_layer_fsm_32 
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
		status_to_link	: 	out std_logic_vector(7 downto 0);
		link_status		:	in std_logic_vector(31 downto 0);
		tx_data_out		:	out std_logic_vector(31 downto 0);
		rx_data_in		:	in std_logic_vector(31 downto 0));

end component;



begin

	dut: t_layer_fsm_32 port map(rst_n => rst_n, clk => clk,
								  write_data => user_data_in,
								  write_address => user_addr_in,
								  user_command => command,
								  status_to_user => status,
								  read_data => user_data_out,
								  read_address => user_addr_out,
								  status_to_link => transport_status,
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
	
	--simulating user process
	app_stim_proc : process
	  begin
		wait for 2 ns;
		rst_n <= '0';
		wait for 2 ns;
		rst_n <= '1';
--		sending_data <= '0';
		wait for 2 ns;
		wait until rising_edge(clk);
		command <= "001";
		user_addr_in <= x"FFFFFFFF";
		sending_data <= '1';
		--wait until rising_edge(clk);--
		for i in 0 to 31 loop
			user_data_in <= std_logic_vector(to_unsigned(i, 32));
			wait until rising_edge(clk);
		end loop;
		wait until rising_edge(clk);
		sending_data <= '0';		
		
		command <= "000";
		wait until rising_edge(clk);
		--wait for read valid flag
		wait until rising_edge(clk);
		for j in 0 to 63 loop
			wait until rising_edge(clk);
		end loop;

		--should be able to send read command now
		
		command <= "010";
		user_addr_in <= x"EEEEEEEE";
		wait until rising_edge(clk);
		for k in 0 to 60 loop
			wait until rising_edge(clk);
		end loop;
		--wait until status(3) = '1';
		--wait until rising_edge(clk);


		
	end process;


	--for link layer
	link_stim_process: process
--		variable j : integer range 0 to 32;
		begin

		--look at link_status logic
		link_status <= x"000000BF";
		wait until rising_edge(clk);
		
		for m in 0 to 15 loop
			wait until rising_edge(clk);
		end loop;
		
		wait until transport_status(5) = '1';
		wait until rising_edge(clk);

		for k in 0 to 10 loop
			wait until rising_edge(clk);
		end loop;
		data_from_link <= x"00000039";
		wait until rising_edge(clk);

--===============================================
		--IF transport layer is done sending data
		--send back status device to host register fis

		wait until transport_status(5) = '0';
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		data_from_link <= x"00000034";
		wait until rising_edge(clk);
--==============================================
--==============================================
--==============================================
--==============================================
		--look at link_status logic
		link_status <= x"000000BF";
		wait until rising_edge(clk);
		
		wait until transport_status(5) = '1';
		wait until rising_edge(clk);

		for k in 0 to 10 loop
			wait until rising_edge(clk);
		end loop;
		data_from_link <= x"00000039";
		wait until rising_edge(clk);

--===============================================
		--IF transport layer is done sending data
		--send back status device to host register fis

		wait until transport_status(5) = '0';
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		wait until rising_edge(clk);
		data_from_link <= x"00000034";
		wait until rising_edge(clk);

		link_status <= x"00000000"; --not allowed to send commands from transport
		wait until rising_edge(clk);
		wait until transport_status(5) = '1';

		wait until rising_edge(clk);		
		link_status <= x"000000BF";

		--wait until rising_edge(clk);

--		while j < 32 loop
--			if(transport_status(0) = '1') then
--				wait until rising_edge(clk);
--				fake_memory(j) <= data_to_link;
--				j := j + 1;
--			else 
--				wait until rising_edge(clk);
--			end if;
			
--		end loop;
		wait until transport_status(6) = '1';
		data_from_link <= x"00000046";
		wait until rising_edge(clk);

		for k in 0 to 15 loop
			data_from_link <= std_logic_vector(to_unsigned(k, 32));
			wait until rising_edge(clk);
		end loop;


		wait until rising_edge(clk);
		data_from_link <= x"00000034";

	end process;

end architecture;
