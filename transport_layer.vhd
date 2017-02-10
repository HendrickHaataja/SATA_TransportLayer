library ieee;                   --! Use standard library.
use ieee.std_logic_1164.all;    --! Use standard logic elements
use ieee.numeric_std.all;       --! Use numeric standard
----------------------------------------------------------------------------
--Status Truth Table:
-- XXX0 == Device Not Ready
-- XXX1 == Device Ready
-- XX01 == Write Not Ready
-- XX11 == Write Ready
-- X0X1 == Send Read Not Ready 
-- X1X1 == Send Ready Ready
-- 0XX1 == Retrieve Read Not Ready
-- 1XX1 == Retrieve Read Ready
	
--Command truth table
-- 000 == Do Nothing
-- X01 == Send Write	 (Command to write data at specified address in SSD)
-- X10 == Send Read		 (Command to retrieve data at specified address from SSD)
-- 1XX == Retrieve Read  (Command to read value from Rx buffer)
----------------------------------------------------------------------------
entity transport_layer is
	generic(DATA_WIDTH : integer := 32);
   port(
		--Interface with Application Layer
		rst_n			:	in std_logic;
		clk			:	in std_logic;
		
		write_data		:	in std_logic_vector(DATA_WIDTH - 1 downto 0);
		write_address	:	in std_logic_vector(DATA_WIDTH - 1 downto 0);
		
		user_command			:	in std_logic_vector(2 downto 0);
		status_to_user			:	out std_logic_vector(3 downto 0); 	
		
		read_data		:	out std_logic_vector(DATA_WIDTH - 1 downto 0);
		read_address	:	out std_logic_vector(DATA_WIDTH - 1 downto 0);
		
		--Interface with Link Layer
		link_status		:	in std_logic_vector(31 downto 0);
		tx_data_out		:	out std_logic_vector(DATA_WIDTH - 1 downto 0);
		rx_data_in		:	in std_logic_vector(DATA_WIDTH - 1 downto 0));

end transport_layer;

architecture transport_layer_arch of transport_layer is
--States for Transport FSM
  type State_Type is (
					--Initial "Main" State	
					transport_idle, 
					
					decode_fis, --Do we need to wait for device signature -- I don't know if we need this?
					
					decode_dma_fis, decode_register_fis, -- Maybe don't need these?
					
					--================================================
					--DMA Write States  --PRELIMINARY
					 dma_write_idle, dma_write_reg_fis_0,
					 dma_write_reg_fis_1, dma_write_reg_fis_2,
					 dma_write_chk_activate, dma_write_data_fis,
					 dma_write_data_frame,	dma_write_chk_status, 
					--================================================						
							
					--================================================
					--DMA Read States --PRELIMINARY
					 dma_read_idle, dma_read_reg_fis_0,
					 dma_read_reg_fis_1, dma_read_reg_fis_2,
					 dma_read_data_fis, dma_read_data_frame,
					 dma_read_chk_status 
					--================================================								
							
					);	
  signal state : State_Type;

--======================================================================================
  -- Type Field Values of supported SATA Frame Information Structures (FIS)
  -- Supported Register FIS Type Field Value
  constant REG_HOST_TO_DEVICE	: std_logic_vector(7 downto 0) := x"27";  --5 Dwords
  constant REG_DEVICE_TO_HOST	: std_logic_vector(7 downto 0) := x"34";
  
  -- Supported DMA FIS Type Field Value
  constant DMA_ACTIVATE_FIS		: std_logic_vector(7 downto 0) := x"39"; --Device to Host -- 1 dword
  --constant DMA_SETUP_FIS		: std_logic_vector(7 downto 0) := x"41"; --Bidirectional  -- 7 dwords --not using
  
  -- Data FIS type field value
  constant DATA_FIS				: std_logic_vector(7 downto 0) := x"46"; --Bidirectional
--======================================================================================

--======================================================================================
--Signals to create Register Host to Device FIS contents
signal fis_type : std_logic_vector(7 downto 0);

--Shadow Registers... Somewhat customized for ease of use
signal feature : std_logic_vector(15 downto 0); -- a reserved field in DMA read ext, DMA write ext. Set to all zeros
signal lba : std_logic_vector(47 downto 0);   --address to write to / read from
signal control : std_logic_vector(7 downto 0);	--Field not defined for DMA read/write ext. Thus is "reserved", set to zeros
signal command : std_logic_vector(7 downto 0);	--35h for dma write ext, 25h dma read ext
signal c_bit 	   : std_logic;					--Set to one when register transfer is due to update of command reg.
signal count : std_logic_vector(15 downto 0);	--# of logical sectors to be transferred for DMA. 0000h indicates 65.536 sectors	
--------------------------------------------------
--	set bit 6 to 1, bit 4 is Transport Dependent, think it should be zero
--Bits 7, 5 are obsolete. Planning on setting to zero
signal device: std_logic_vector(7 downto 0);	
--------------------------------------------------



signal i_bit		: std_logic;                    --used only for device to host
signal status		: std_logic_vector(7 downto 0); --used only for device to host
signal error		: std_logic_vector(7 downto 0); --used only for device to host
--======================================================================================


--Record type for Host to Device register FIS
type register_fis_type	is
  record
	fis_type :	std_logic_vector(7 downto 0);		--set to 27h
	crrr_pm	 :	std_logic_vector(7 downto 0); 		--set to 80h	
	command : std_logic_vector(7 downto 0);			--see command support constants
	features : std_logic_vector(7 downto 0);  		--set to 00h
	lba : std_logic_vector(23 downto 0);			--lower half of address
	device: std_logic_vector(7 downto 0);     		--set to 40h?
	lba_ext : std_logic_vector(23 downto 0);		--upper half of address
	features_ext : std_logic_vector(7 downto 0); 	--set to 00h
	count : std_logic_vector(15 downto 0);			--Will be size of tx buffer
	icc	  : std_logic_vector(7 downto 0); 			--set to 00h
	control : std_logic_vector(7 downto 0); 		--set to 00h
	aux		: std_logic_vector(31 downto 0);		--set to all zeros
end record;

type register_fis_array_type is array (1 downto 0) of register_fis_type;
signal tx_fis_array, rx_fis_array	:	register_fis_array_type;

--signal rx_reg_fis	:	register_fis_type;

--======================================================================================
--Supported ATA Commands
--need EXT commands to support 48 bit address
constant READ_DMA_EXT	: std_logic_vector(7 downto 0) := x"25";
constant WRITE_DMA_EXT	: std_logic_vector(7 downto 0) := x"35";
--======================================================================================


--======================================================================================
--Constants for user commands --NOT USING BECAUSE DON'T CARES DON'T WORK IN VHDL
--constant SEND_WRITE		: std_logic_vector (2 downto 0) := "X01";
--constant SEND_READ  	: std_logic_vector (2 downto 0)	:= "X10";
--constant RETRIEVE_READ	: std_logic_vector (2 downto 0)	:= "1XX";
--======================================================================================

--======================================================================================
--Signals to control buffers
signal tx0_locked, tx1_locked, rx0_locked, rx1_locked : std_logic; -- Custom signal to allow SM to take control of buffers
signal tx_index : integer range 0 to 1; -- custom signal to use as index to array of tx register FISs
signal rx_index : integer range 0 to 1; -- custom signal to use as index to array of tx register FISs

--array type: holds two std_logic_vectors of sys data width
type data_width_array_type is array (1 downto 0) of std_logic_vector(DATA_WIDTH - 1 downto 0); 

--signals have latency of one clock cycle using area optimization... especially rdreq to q[]!
signal tx_data : data_width_array_type;
signal tx_rdreq : std_logic_vector(1 downto 0);
signal tx_sclr : std_logic_vector(1 downto 0);
signal tx_wrreq : std_logic_vector(1 downto 0);
signal tx_empty : std_logic_vector(1 downto 0);
signal tx_full : std_logic_vector(1 downto 0);
signal tx_q : data_width_array_type;

signal rx_data : data_width_array_type;
signal rx_rdreq : std_logic_vector(1 downto 0);
signal rx_sclr : std_logic_vector(1 downto 0);
signal rx_wrreq : std_logic_vector(1 downto 0);
signal rx_empty : std_logic_vector(1 downto 0);
signal rx_full : std_logic_vector(1 downto 0);
signal rx_q : data_width_array_type;
--======================================================================================
					signal tx_read_request, rx_read_request : std_logic_vector(1 downto 0);
					
					--temporary signal for testing
					signal tx0_read_valid, tx1_read_valid, rx0_read_valid, rx1_read_valid : std_logic;

--Component declarations
component data_buffer_w32_d32
	PORT
	(
		clock		: IN STD_LOGIC ;
		data		: IN STD_LOGIC_VECTOR (DATA_WIDTH - 1 DOWNTO 0);
		rdreq		: IN STD_LOGIC ;
		sclr		: IN STD_LOGIC ;
		wrreq		: IN STD_LOGIC ;
		empty		: OUT STD_LOGIC ;
		full		: OUT STD_LOGIC ;
		q		: OUT STD_LOGIC_VECTOR (DATA_WIDTH - 1 DOWNTO 0)
	);
END component;


begin

tx0_data_buffer: data_buffer_w32_d32 port map(clock => clk, data => tx_data(0), rdreq => tx_rdreq(0), 
			 sclr => tx_sclr(0), wrreq => tx_wrreq(0), empty => tx_empty(0),
			 full => tx_full(0), q => tx_q(0));
tx1_data_buffer: data_buffer_w32_d32 port map(clock => clk, data => tx_data(1), rdreq => tx_rdreq(1), 
			 sclr => tx_sclr(1), wrreq => tx_wrreq(1), empty => tx_empty(1),
			 full => tx_full(1), q => tx_q(1));			 
rx0_data_buffer: data_buffer_w32_d32 port map(clock => clk, data => rx_data(0), rdreq => rx_rdreq(0), 
			 sclr => rx_sclr(0), wrreq => rx_wrreq(0), empty => rx_empty(0),
			 full => rx_full(0), q => rx_q(0));
rx1_data_buffer: data_buffer_w32_d32 port map(clock => clk, data => rx_data(1), rdreq => rx_rdreq(1), 
			 sclr => rx_sclr(1), wrreq => rx_wrreq(1), empty => rx_empty(1),
			 full => rx_full(1), q => rx_q(1));
--=================================================================================================================
--Process to control the flow of user data into the tx buffers
--The dual-buffer system allows user data to be written to a buffer even when the Transort FSM is performing a write command

	--tx_wrreq(0) <= '1' when (user_command(1 downto 0) = "01" and tx_full(0) = '0' and tx0_locked = '0' and tx_wrreq(1) = '0') else '0'; --when others;
	--tx_wrreq(1) <= '1' when (user_command(1 downto 0) = "01" and tx_full(1) = '0' and tx1_locked = '0' and tx_wrreq(0) = '0') else '0';-- when others;

	tx_buffer_control	: process(clk,rst_n)
	  begin
		if(rst_n = '0') then
			--reset stuff 
			tx_sclr(0) <= '1';
			tx_sclr(1) <= '1';
			--tx0_locked <= '0';
			--tx1_locked <= '0';
			tx_wrreq(0) <= '0';
			tx_wrreq(1) <= '0';
			--temporary signal for testing
			tx0_read_valid <= '0';
			tx1_read_valid <= '0';
			--reset in use flags? or use full signals?
		elsif(rising_edge(clk)) then
			tx_sclr(0) <= '0';
			tx_sclr(1) <= '0';
			if(user_command(1 downto 0) = "01") then --user is sending data
				if(tx_full(0) = '0' and tx0_locked = '0') then
					--add user data to buffer
					tx_wrreq(0) <= '1';
					tx0_read_valid <= '0';
					tx1_read_valid <= '1';
					tx_data(0)  <= write_data;		
					tx_wrreq(1) <= '0'; 
				elsif(tx_full(1) = '0' and tx1_locked = '0') then
					--add user data to buffer
					--temporary signal for testing
					tx_wrreq(0) <= '0';
					tx0_read_valid <= '1';
					tx1_read_valid <= '0';
					tx_wrreq(1) <= '1';
					tx_data(1) <= write_data; 
				else
				--Error: Should not get here unless source is overwhelming controller
				end if;
			else
				tx_wrreq(0) <= '0';
				tx_wrreq(0) <= '0';
			end if;
		end if;
	end process;

--Do something like: IsWriteValid <= tx0_buffer_in_use || tx1_buffer_in_use;
--=================================================================================================================
--old, keep for now as a reference 
	--rx_buffer_control	: process(clk, rst_n)
	--  begin
	--	if(rst_n = '0') then
	--		rx_sclr(0) <= '1';
	--		rx_sclr(1) <= '1';
	--		rx0_locked <= '0';
	--		rx1_locked <= '0';
	--		rx_rdreq(0) <= '0';
	--		rx_rdreq(1) <= '0';
	--	elsif(rising_edge(clk)) then
	--		rx_sclr(0) <= '0';
	--		rx_sclr(1) <= '0';		
	--		if(user_command(1 downto 0) = "10") then
	--			if(rx_empty(0) = '0' and rx0_locked = '0') then
	--				rx_rdreq(0) <= '1';
	--				read_data <= rx_q(1);
	--			elsif(rx_empty(1) = '0' and rx1_locked = '0') then
	--				rx_rdreq(1) <= '1';
	--				read_data <= rx_q(1); 
	--			else
	--				--Error: Should not get here unless source is overwhelming controller
	--			end if;
	--		end if;
	--	end if;
	--end process;
--new for test, use to develop actual process

	rx_buffer_control_reads : process(clk, rst_n)
	  begin
		if(rst_n = '0') then
--			rx_sclr(0) <= '1';
--			rx_sclr(1) <= '1';
--			rx0_locked <= '0';
--			rx1_locked <= '0';
			rx_rdreq(0) <= '0';
			rx_rdreq(1) <= '0';
			rx_read_request(0) <= '0';
			rx_read_request(1) <= '0';			
		elsif(rising_edge(clk)) then
			--rx_sclr(0) <= '0';
			--rx_sclr(1) <= '0';		
			if(user_command(2) = '1') then
				if(rx_empty(0) = '0' and rx0_locked = '0')then --and rx0_read_valid = '1') then
					rx_rdreq(0) <= '1';
					rx_read_request(0) <= '1';
					if(rx_read_request(0) = '1') then
						read_data <= rx_q(0);
					end if;
				elsif(rx_empty(1) = '0' and rx1_locked = '0')then --and rx1_read_valid = '1') then
					rx_rdreq(1) <= '1';
					rx_read_request(1) <= '1';
					if(rx_read_request(1) = '1') then
						read_data <= rx_q(1); 
					end if;
				--else
					--Error: Should not get here unless source is overwhelming controller
				end if; --something
			else
	
				rx_rdreq(0) <= '0';
				rx_rdreq(1) <= '0';
				if(rx_empty(0) = '1' and rx_empty(1) = '1') then
					--status(3) <= '0';	
				end if;
				rx_read_request(0) <= '0';
				rx_read_request(1) <= '0';
			end if;
		end if;
	end process;

--============================================================================
--============================================================================
--test rx buf process for stuff
	test_buf_sm : process(clk, rst_n)
	  begin
		if(rst_n = '0') then
			--reset stuff 
			rx_sclr(0) <= '1';
			rx_sclr(1) <= '1';
			rx0_locked <= '0';
			rx1_locked <= '0';
			rx_wrreq(0) <= '0';
			rx_wrreq(1) <= '0';
			
			--tx_sclr(0) <= '1';
			--tx_sclr(1) <= '1';
			tx0_locked <= '0';
			tx1_locked <= '0';
			tx_rdreq(0) <= '0';
			tx_rdreq(1) <= '0';
			
			--temporary signal for testing
			rx0_read_valid <= '0';
			rx1_read_valid <= '0';
			state <= transport_idle;
			--reset in use flags? or use full signals?
		elsif(rising_edge(clk)) then
			rx_sclr(0) <= '0';
			rx_sclr(1) <= '0';

			--tx_sclr(0) <= '0';
			--tx_sclr(1) <= '0';


		case (state) is
		-----------------------------------------------	-----------------------------------------------	
			-- Idle SM states (top level)
		-----------------------------------------------	-----------------------------------------------		
		when transport_idle =>
		

			--if (link_status = fis_received) then
				--state <= decode_fis;	--is this how we should do this?				
				
			--els
			if (tx_full(0) = '1') then	--User is sending "Write" command --Don't transition to DMA Write until a buffer is full
				--Build register FIS?
				--lock tx0 buffer
				tx0_locked <= '1';
				--set buffer index to zero
				tx_index <= 0;
				--Proceed to DMA Write
				state <= dma_write_idle;
			elsif (tx_full(1) = '1') then
				--Build register FIS?
				--lock tx1 buffer
				tx1_locked <= '1';
				--set buffer index to 1
				tx_index <= 1;
				--Proceed to DMA Write
				state <= dma_write_idle;
						
			elsif (command(1 downto 0)= "10") then
				--lock rx buffer
				--rx_locked = '1';
				--build read register FIS?
				state <= dma_read_idle;
			end if;
		
		when dma_read_idle =>
			if(rx_full(0) = '0') then
				rx_index <= 0;
				rx_wrreq(0) <= '1';
				rx0_locked <= '1';
				state <= dma_read_data_frame;
			elsif(rx_full(1) = '0') then
				rx_index <= 1;
				rx_wrreq(1) <= '1';
				rx1_locked <= '1';
				state <= dma_read_data_frame;			
			end if;
	
		when dma_read_data_frame =>
			if(rx_full(rx_index) = '0') then-- and data_valid = '1') then --data valid flag saying data from link is coming
				rx_data(rx_index) <= rx_data_in;
			else
				rx_wrreq(rx_index) <= '0';
				if(rx_full(rx_index mod 1) = '0') then
					if(rx_index = 0) then
						rx_index <= 1;
						rx_wrreq(1) <= '1';
						rx1_locked <= '1';
						rx0_locked <= '0';
					else
						rx_index <= 0;
						rx_wrreq(0) <= '1';
						rx0_locked <= '1';						
						rx1_locked <= '0';
					end if;
				end if;
			end if;
			 
		when dma_write_idle =>
			--if(link_busy = '0') then
				tx_rdreq(tx_index) <= '1';
				state <= dma_write_data_frame;
			--end if;
		
		when dma_write_data_frame =>
			if(tx_empty(tx_index) = '0') then
				tx_data_out <= tx_q(tx_index);
			else
				tx_rdreq(tx_index) <= '0';
				state <= transport_idle;
				if(tx_index = 0) then
					tx0_locked <= '0';
				else
					tx1_locked <= '0';
				end if;
			end if;
		when others =>  state <= transport_idle;
		end case;
	  end if;
	end process;
--============================================================================
--============================================================================
	--update status vectors
	status_to_user(0) <= '1'; --having device always be ready for now
	status_to_user(1) <= '1' when (tx_full(0) = '0' or tx_full(1) = '0') else '0';
	status_to_user(2) <= '1' when (rx_full(0) = '0' or rx_full(1) = '0') else '0';
	status_to_user(3) <= '1' when (rx_empty(0) = '0' or rx_empty(1) = '0') else '0';


end architecture;