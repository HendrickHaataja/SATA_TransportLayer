library ieee;                   --! Use standard library.
use ieee.std_logic_1164.all;    --! Use standard logic elements
use ieee.numeric_std.all;       --! Use numeric standard

use work.transport_layer_pkg.all;
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
		status_to_link :	out std_logic_vector(7 downto 0); --for test just use bit 0 to indicate data ready
		link_status		:	in std_logic_vector(31 downto 0);
		tx_data_out		:	out std_logic_vector(DATA_WIDTH - 1 downto 0);
		rx_data_in		:	in std_logic_vector(DATA_WIDTH - 1 downto 0));

end transport_layer;

architecture transport_layer_arch of transport_layer is
--States for Transport FSM
	
  signal current_state, next_state : State_Type; 

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
	--Bits 7, 5 are obsolete? Currently planning on setting to zero
	signal device: std_logic_vector(7 downto 0);	
	--------------------------------------------------
	signal i_bit		: std_logic;                    --used only for device to host
	signal status		: std_logic_vector(7 downto 0); --used only for device to host
	signal error		: std_logic_vector(7 downto 0); --used only for device to host
	--======================================================================================

	signal tx_fis_array, rx_fis_array	:	register_fis_array_type; -- signals to hold host to device register FIS contents

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

	--signals have latency of one clock cycle using area optimization... especially rdreq to q[]!
	signal tx_data : data_width_array_type;
	signal tx_rdreq : std_logic_vector(1 downto 0);
	signal tx_sclr : std_logic_vector(1 downto 0);
	signal tx_wrreq : std_logic_vector(1 downto 0);
	signal tx_almost_empty : std_logic_vector(1 downto 0);
	signal tx_almost_full : std_logic_vector(1 downto 0);
	signal tx_empty : std_logic_vector(1 downto 0);
	signal tx_full : std_logic_vector(1 downto 0);
	signal tx_q : data_width_array_type;

	signal rx_data : data_width_array_type;
	signal rx_rdreq : std_logic_vector(1 downto 0);
	signal rx_sclr : std_logic_vector(1 downto 0);
	signal rx_wrreq : std_logic_vector(1 downto 0);
	signal rx_almost_empty : std_logic_vector(1 downto 0);
	signal rx_almost_full : std_logic_vector(1 downto 0);
	signal rx_empty : std_logic_vector(1 downto 0);
	signal rx_full : std_logic_vector(1 downto 0);
	signal rx_q : data_width_array_type;
	--======================================================================================
						
	signal tx_read_request, rx_read_request : std_logic_vector(1 downto 0);
						
	--temporary signal for testing
	signal tx0_read_valid, tx1_read_valid, rx0_read_valid, rx1_read_valid : std_logic;
	
	--test link interface status signals
	signal link_is_idle : std_logic;

	--signals to get this to compile
	signal fis_received : std_logic;
	signal link_fis_rdy : std_logic;
	signal link_fis_type : std_logic_vector(7 downto 0);

	--CONSTANTS UPDATE WITH VALUES
	constant STATUS_ERR : integer := 0;
	constant STATUS_DF : integer := 0;
	constant STATUS_BSY : integer := 0;

begin
--=================================================================================================================
--Transport Layer Finite State Machine
--=================================================================================================================
	transport_state_memory	:	process(clk, rst_n)
	  begin
		if(rst_n = '0') then
			current_state <= transport_idle;
		elsif(rising_edge(clk)) then
			current_state <= next_state;
		end if;
	end process;

	transport_next_state_logic: process (current_state, link_status, tx_full, tx_empty, command)
      begin 
	  
		case (current_state) is
		-----------------------------------------------	-----------------------------------------------	
			-- Idle SM states (top level)
		-----------------------------------------------	-----------------------------------------------		
			when transport_idle => 
				if (link_status = x"00000000") then --FIS RECEIVED
					next_state <= decode_fis;	--is this how we should do this?				
				elsif (tx_full(tx_index) = '1') then	--User is sending "Write" command --Don't transition to DMA Write until a buffer is full
					next_state <= dma_write_idle;
				elsif (tx_full(tx_index) = '1') then
					next_state <= dma_write_idle;
				elsif (command(2 downto 0) = "10") then
					next_state <= dma_read_idle;
				elsif (command(2)='1') then
					--next_state <= read_rx_buffer;
				else 
					next_state <= transport_idle;
				end if;
	-----------------------------------------------	-----------------------------------------------					
--========================================================================================				
				-- DMA Write EXT SM states
				--TODO: check last states, create missing signals
					--tx_data_out, use tx_data_out or need intermediary signal?
			when dma_write_idle	  	=> 
				if (link_is_idle = '1') then --#UPDATE
					next_state <= dma_write_reg_fis_0;
				else 
					next_state <= dma_write_idle;
				end if;
			when dma_write_reg_fis_0	=>	next_state <= dma_write_reg_fis_1;
			when dma_write_reg_fis_1	=>	next_state <= dma_write_reg_fis_2;
			when dma_write_reg_fis_2	=> 	next_state <= dma_write_chk_activate;
			when dma_write_chk_activate	=> 
				if(link_status = x"00000000") then --FIS RECEIVED
					if(link_fis_type = DMA_ACTIVATE_FIS) then			 
						next_state <= dma_write_data_fis;	--start transmitting data FIS
					--elsif(error) then
						--give up
					end if;
				end if;
			when dma_write_data_fis	=>	next_state <= dma_write_data_frame;
			when dma_write_data_frame	=> ----UPDATE THIS STATE
				if(tx_empty(tx_index) = '0') then
					--tx0_rdreq(tx_index) <= '1'; --will this update in time?
				else
					next_state <= dma_write_chk_status;
				end if;
			when dma_write_chk_status	=>  ----UPDATE THIS STATE
				if(link_fis_rdy = '1' and rx_data_in (7 downto 0) = REG_DEVICE_TO_HOST) then
					--check error bit and device fault bit in the  Status field.. if error is asserted can check error field
					--TODO: create constants for ERROR, DEV_FAULT, etc
					if(rx_data_in(STATUS_ERR) = '1' or rx_data_in(STATUS_DF) = '1') then
					--error occured
					elsif(rx_data_in(STATUS_BSY) = '0') then
						next_state <= transport_idle;	--Go back to transport idle?
					else
						--Loop here or go somewhere else?
					end if;
				--elsif(error) then
					--give up
				end if;					
--========================================================================================					
			-- DMA Read EXT SM states
			--TODO: create next_state logic/assignments, check last states, create missing signals
			when dma_read_idle	  	=> 
				if (link_is_idle = '1') then --					
					next_state <= dma_read_reg_fis_0;
				else 
					next_state <= dma_read_idle;
				end if;
			when dma_read_reg_fis_0	=> 	next_state <= dma_read_reg_fis_1;
			when dma_read_reg_fis_1	=>	next_state <= dma_read_reg_fis_2;
			when dma_read_reg_fis_2	=>	next_state <= dma_read_data_fis;	
			when dma_read_data_fis	=> 
				if(link_fis_rdy = '1' and rx_data_in(7 downto 0)= DATA_FIS) then
					next_state <= dma_read_data_frame;
				end if;
			when dma_read_data_frame	=> 
				--if(rx_data_in_valid and rx_buffer_not_full) then
					--rx_buffer_in <= rx_data_in;
				--else
					next_state <= dma_read_chk_status;
				--end if;
			when dma_read_chk_status	=>  ----UPDATE THIS STATE
				if(link_status = x"00000000") then	--DONE		 
					next_state <= transport_idle;	--Go back to transport idle?
	--			elsif(error) then
	--				give up
				end if;					
--=======================================================================================
			when others =>  next_state <= transport_idle;
		end case;
    end process;
--=================================================================================================================

	transport_output_logic:	process (current_state, link_status, tx_full, tx_empty, command)
	  begin 
		case (current_state) is
		-----------------------------------------------	-----------------------------------------------	
			-- Idle SM states (top level)
		-----------------------------------------------	-----------------------------------------------		
			when transport_idle => 
				if (link_status = x"00000000") then --FIS RECEIVED
						
				elsif (tx_full(0) = '1') then	--User is sending "Write" command --Don't transition to DMA Write until a buffer is full
					--Build register FIS?
					--lock tx0 buffer
					tx0_locked <= '1';
					--set buffer index to zero
					tx_index <= 0;
					--Proceed to DMA Write
				elsif (tx_full(1) = '1') then
						--Build register FIS?
						--lock tx1 buffer
						tx1_locked <= '1';
						--set buffer index to 1
						tx_index <= 1;			
				elsif (command(1 downto 0) = "10") then
					--lock rx buffer
					--rx_locked <= '1';
					--build read register FIS?
								
					-----------------------------------------------
					--Think about this state, can we truly read from RX buffer while sending data?
				elsif (command(2)='1') then
					
					-----------------------------------------------
				else 
				end if;
	-----------------------------------------------	-----------------------------------------------					
	
--========================================================================================				
			-- DMA Write EXT SM states
			--TODO: check last states, create missing signals
				--tx_data_out, use tx_data_out or need intermediary signal?
			when dma_write_idle	  	=> 
				if (link_is_idle = '1') then --#UPDATE
					--start tranmsitting Register FIS
				else 
					--wait for Link
					--state <= dma_write_reg_fis_0;
				end if;
			when dma_write_reg_fis_0	=> 
				
				tx_data_out <= tx_fis_array(tx_index).device & tx_fis_array(tx_index).lba & 
				tx_fis_array(tx_index).features & tx_fis_array(tx_index).command &
				tx_fis_array(tx_index).crrr_pm & tx_fis_array(tx_index).fis_type;
				
			when dma_write_reg_fis_1	=> 
				tx_data_out <= tx_fis_array(tx_index).control & tx_fis_array(tx_index).icc &
							 tx_fis_array(tx_index).count & tx_fis_array(tx_index).features_ext &	
							 tx_fis_array(tx_index).lba_ext;

			when dma_write_reg_fis_2	=> 
				tx_data_out <= tx_fis_array(tx_index).aux & tx_fis_array(tx_index).aux;
					--SET DEVICE BUSY BIT?			 
					
			when dma_write_chk_activate	=> 
				if(link_status = x"00000000") then --FIS RECEIVED
					if(link_fis_type = DMA_ACTIVATE_FIS) then			 
				--	elsif(error) then
				--		give up
					end if;
				end if;
					
			when dma_write_data_fis	=> 
				tx_data_out <=  "000000" & DATA_FIS; --padding with 32 bits of extra zeros, because of system data width
				tx_rdreq(tx_index) <= '1';-- Need to update rdreq signal a clock cycle before data is desired
				
			when dma_write_data_frame	=> ----UPDATE THIS STATE
				if(tx_empty(tx_index) = '0') then
					--tx0_rdreq(tx_index) <= '1'; --will this update in time?
					tx_data_out <= tx_q(tx_index);	--start transmitting data frame
				else
					tx_rdreq(tx_index) <= '0'; 
				end if;
				
			when dma_write_chk_status	=>  ----UPDATE THIS STATE
				if(link_fis_rdy = '1' and rx_data_in (7 downto 0) = REG_DEVICE_TO_HOST) then
					--check error bit and device fault bit in the  Status field.. if error is asserted can check error field
					--TODO: create constants for ERROR, DEV_FAULT, etc
					if(rx_data_in(STATUS_ERR) = '1' or rx_data_in(STATUS_DF) = '1') then
						--error occured
					elsif(rx_data_in(STATUS_BSY) = '0') then
					else
						--Loop here or go somewhere else?
					end if;
				--elsif(error) then
				--	give up
				end if;					
--========================================================================================					
			-- DMA Read EXT SM states
			--TODO: create state logic/assignments, check last states, create missing signals
			when dma_read_idle	  	=> 
				if (link_is_idle = '1') then
					--transmit register fis
				else 
				end if;
			when dma_read_reg_fis_0	=> 	--Send first part of host to device reg fis
				tx_data_out <= rx_fis_array(rx_index).device & rx_fis_array(rx_index).lba & 
				rx_fis_array(rx_index).features & rx_fis_array(rx_index).command &
				rx_fis_array(rx_index).crrr_pm & rx_fis_array(rx_index).fis_type;		
									
				when dma_read_reg_fis_1	=> --Send second part of host to device reg fis
					tx_data_out <= rx_fis_array(rx_index).control & rx_fis_array(rx_index).icc &
					rx_fis_array(rx_index).count & rx_fis_array(rx_index).features_ext &	
					rx_fis_array(rx_index).lba_ext;					
					
				when dma_read_reg_fis_2	=> --Send final part of host to device reg fis
					tx_data_out <= rx_fis_array(rx_index).aux & rx_fis_array(rx_index).aux;
					--SET DEVICE BUSY BIT?
				
				when dma_read_data_fis	=> 
					if(link_fis_rdy = '1' and rx_data_in(7 downto 0)= DATA_FIS) then
					end if;
				when dma_read_data_frame	=> 
					--if(rx_data_in_valid and rx_buffer_not_full) then
						--rx_buffer_in <= rx_data_in;
					--else

					--end if;
				when dma_read_chk_status	=>  ----UPDATE THIS STATE
					if(link_status = x"00000000") then			 

					--elsif(error) then
					--	give up
					end if;					
--========================================================================================
				when others => -- state <= transport_idle;
			end case;
	end process;

--=================================================================================================================
--Process to control the flow of user data into the tx buffers
--The dual-buffer system allows user data to be written to a buffer even when the Transort FSM is performing a write command
	tx_buffer_control	: process(clk,rst_n)
	  begin
		if(rst_n = '0') then
			--reset stuff 
			tx_sclr(0) <= '1';
			tx_sclr(1) <= '1';
			--reset in use flags? or use full signals?
		elsif(rising_edge(clk)) then
			tx_sclr(0) <= '0';
			tx_sclr(1) <= '0';
			if(command(1 downto 0) = "01") then --user is sending data
				if(tx_full(0) = '0' or tx0_locked = '0') then
					--add user data to buffer
					tx_wrreq(0) <= '1';
					tx_data(0)  <= write_data; 
				elsif(tx_full(1) = '0' or tx1_locked = '0') then
					--add user data to buffer
					tx_wrreq(1) <= '1';
					tx_data(1) <= write_data; 
				else
				--Error: Should not get here unless source is overwhelming controller
				end if;
			end if;
		end if;
	end process;
--Do something like: IsWriteValid <= tx0_buffer_in_use || tx1_buffer_in_use;
--=================================================================================================================

	rx_buffer_control	: process(clk, rst_n)
	  begin
		if(rst_n = '0') then
			rx_sclr(0) <= '1';
			rx_sclr(1) <= '1';
		elsif(rising_edge(clk)) then
			rx_sclr(0) <= '0';
			rx_sclr(1) <= '0';		
			if(command(1 downto 0) = "10") then --user is sending data
				if(rx_empty(0) = '0' and rx0_locked = '0') then
					rx_rdreq(0) <= '1';
					read_data <= rx_data(1);
				elsif(rx_empty(1) = '0' and rx1_locked = '0') then
					rx_rdreq(1) <= '1';
					read_data <= rx_data(1); 
				else
					--Error: Should not get here unless source is overwhelming controller
				end if;
			end if;
		end if;
	end process;

end architecture;