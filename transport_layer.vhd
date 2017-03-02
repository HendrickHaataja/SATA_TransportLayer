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
-- X01 == Send Write     (Command to write data at specified address in SSD)
-- X10 == Send Read      (Command to retrieve data at specified address from SSD)
-- 1XX == Retrieve Read  (Command to read value from Rx buffer)
----------------------------------------------------------------------------
entity t_layer_fsm_32 is
   port(
        --Interface with Application Layer
        rst_n           :   in std_logic;
        clk         :   in std_logic;

        write_data      :   in std_logic_vector(DATA_WIDTH - 1 downto 0);
        write_address   :   in std_logic_vector(DATA_WIDTH - 1 downto 0);

        user_command            :   in std_logic_vector(2 downto 0);
        status_to_user          :   out std_logic_vector(3 downto 0);

        read_data       :   out std_logic_vector(DATA_WIDTH - 1 downto 0);
        read_address    :   out std_logic_vector(DATA_WIDTH - 1 downto 0);

        --Interface with Link Layer
        status_to_link :    out std_logic_vector(7 downto 0); --for test just use bit 0 to indicate data ready
        link_status     :   in std_logic_vector(31 downto 0);
        tx_data_out     :   out std_logic_vector(DATA_WIDTH - 1 downto 0);
        rx_data_in      :   in std_logic_vector(DATA_WIDTH - 1 downto 0));

end t_layer_fsm_32;

architecture t_layer_fsm_32_arch of t_layer_fsm_32 is
--States for Transport FSM

  signal current_state, next_state : State_Type;

    --======================================================================================
    --Signals to create Register Host to Device FIS contents
    signal fis_type : std_logic_vector(7 downto 0);

    --Shadow Registers... Somewhat customized for ease of use
    signal feature : std_logic_vector(15 downto 0); -- a reserved field in DMA read ext, DMA write ext. Set to all zeros
    signal lba : std_logic_vector(47 downto 0);   --address to write to / read from
    signal control : std_logic_vector(7 downto 0);  --Field not defined for DMA read/write ext. Thus is "reserved", set to zeros
    signal command : std_logic_vector(7 downto 0);  --35h for dma write ext, 25h dma read ext
    signal c_bit       : std_logic;                 --Set to one when register transfer is due to update of command reg.
    signal count : std_logic_vector(15 downto 0);   --# of logical sectors to be transferred for DMA. 0000h indicates 65.536 sectors --not currently using
    --------------------------------------------------
    --  set bit 6 to 1, bit 4 is Transport Dependent, think it should be zero
    --Bits 7, 5 are obsolete? Currently planning on setting to zero
    signal device: std_logic_vector(7 downto 0);
    --------------------------------------------------
    signal i_bit        : std_logic;                    --used only for device to host
    signal status       : std_logic_vector(7 downto 0); --used only for device to host
    signal error        : std_logic_vector(7 downto 0); --used only for device to host
    --======================================================================================

    signal tx_fis_array, rx_fis_array   :   register_fis_array_type; -- signals to hold host to device register FIS contents

    --======================================================================================
    --Constants for user commands --NOT USING BECAUSE DON'T CARES DON'T WORK IN VHDL
    --constant SEND_WRITE       : std_logic_vector (2 downto 0) := "X01";
    --constant SEND_READ    : std_logic_vector (2 downto 0) := "X10";
    --constant RETRIEVE_READ    : std_logic_vector (2 downto 0) := "1XX";
    --======================================================================================

    --Buffer arrays
    type buffer_array is array(15 downto 0) of std_logic_vector(DATA_WIDTH - 1 downto 0);
    type double_buffer is array (1 downto 0) of buffer_array;

    signal tx_buffer : double_buffer;
    signal rx_buffer : double_buffer;

    signal tx_write_ptr, tx_read_ptr : integer range 0 to DATA_WIDTH - 1;
    signal rx_write_ptr, rx_read_ptr : integer range 0 to DATA_WIDTH - 1;


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
    signal link_rdy : std_logic;
    signal pause    : std_logic;

    signal paused_data_to_link : std_logic_vector(DATA_WIDTH - 1 downto 0);

    --signals to get this to compile
    signal fis_received : std_logic;
    signal link_fis_rdy : std_logic;
    signal link_fis_type : std_logic_vector(7 downto 0);

    --CONSTANTS UPDATE WITH CORRECT VALUES!!!!!
    constant STATUS_ERR : integer := 0;
    constant STATUS_DF : integer := 0;
    constant STATUS_BSY : integer := 0;

begin

    --buffer component instantiations
    tx0_data_buffer: data_bufer_w32_d16_extended port map(clock => clk, data => tx_data(0), rdreq => tx_rdreq(0),
                 sclr => tx_sclr(0), wrreq => tx_wrreq(0), almost_empty => tx_almost_empty(0),
                 almost_full => tx_almost_full(0), empty => tx_empty(0), full => tx_full(0), q => tx_q(0));
    tx1_data_buffer: data_bufer_w32_d16_extended port map(clock => clk, data => tx_data(1), rdreq => tx_rdreq(1),
                 sclr => tx_sclr(1), wrreq => tx_wrreq(1), almost_empty => tx_almost_empty(1),
                 almost_full => tx_almost_full(1), empty => tx_empty(1), full => tx_full(1), q => tx_q(1));
    rx0_data_buffer: data_bufer_w32_d16_extended port map(clock => clk, data => rx_data(0), rdreq => rx_rdreq(0),
                 sclr => rx_sclr(0), wrreq => rx_wrreq(0), almost_empty => rx_almost_empty(0),
                 almost_full => rx_almost_full(0), empty => rx_empty(0), full => rx_full(0), q => rx_q(0));
    rx1_data_buffer: data_bufer_w32_d16_extended port map(clock => clk, data => rx_data(1), rdreq => rx_rdreq(1),
                 sclr => rx_sclr(1), wrreq => rx_wrreq(1), almost_empty => rx_almost_empty(1),
                 almost_full => rx_almost_full(1), empty => rx_empty(1), full => rx_full(1), q => rx_q(1));
--=================================================================================================================
--Transport Layer Finite State Machine
--=================================================================================================================
--=================================================================================================================
--Transport Layer Finite State Machine
--=================================================================================================================
    transport_state_memory  :   process(clk, rst_n)
      begin
        if(rst_n = '0') then
            current_state <= transport_reset;
        elsif(rising_edge(clk)) then
            current_state <= next_state;
        end if;
    end process;

    transport_next_state_logic: process (current_state, link_status, link_rdy, rx_data_in,link_fis_type, tx_full, tx_almost_full, tx_almost_empty, tx_empty, user_command,rst_n,
                                        rx_full, rx_almost_full, rx_empty, rx_almost_empty, pause, tx_index, rx_index, link_fis_rdy)
      begin

        case (current_state) is
        ----------------------------------------------- -----------------------------------------------
            -- Idle SM states (top level)
        ----------------------------------------------- -----------------------------------------------
            when transport_reset =>
                if(rst_n = '0') then
                    next_state <= transport_reset;
                else
                    next_state <= transport_idle;
                end if;
            when transport_idle =>
                --if (link_status = x"00000001") then --FIS RECEIVED
                    --next_state <= decode_fis; --is this how we should do this?
                if (tx_full(tx_index) = '1') then   --User is sending "Write" command --Don't transition to DMA Write until a buffer is full
                    next_state <= dma_write_idle;
                elsif (tx_full(tx_index) = '1') then
                    next_state <= dma_write_idle;
                elsif (user_command(1 downto 0) = "10") then
                    next_state <= dma_read_idle;
                --elsif (command(2)='1') then
                    --next_state <= read_rx_buffer;
                else
                    next_state <= transport_idle;
                end if;
    ----------------------------------------------- -----------------------------------------------
--========================================================================================
                -- DMA Write EXT SM states
            when dma_write_idle     =>
                --wait until link is idle before sending FIS
                --Should pause check be here?
                next_state <= dma_write_reg_fis_0;
            when dma_write_reg_fis_0    =>
                if(link_rdy = '1' and pause = '0')then
                    next_state <= dma_write_reg_fis_1;
                else
                    next_state <= dma_write_reg_fis_0;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_write_reg_fis_1    =>
                if(pause = '0')then
                    next_state <= dma_write_reg_fis_2;
                else
                    next_state <= dma_write_reg_fis_1;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_write_reg_fis_2    =>
                if(pause = '0')then
                    next_state <= dma_write_reg_fis_3;
                else
                    next_state <= dma_write_reg_fis_2;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_write_reg_fis_3    =>
                if(pause = '0')then
                    next_state <= dma_write_reg_fis_4;
                else
                    next_state <= dma_write_reg_fis_3;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_write_reg_fis_4    =>
                if(pause = '0')then
                    next_state <= dma_write_chk_activate;
                else
                    next_state <= dma_write_reg_fis_4;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_write_chk_activate => --update to ensure state waits for activate FIS to proceed
                if(link_fis_type = DMA_ACTIVATE_FIS) then
                    next_state <= dma_write_data_idle;
                else
                    next_state <= dma_write_chk_activate;
                end if;
            when dma_write_data_idle => --Activate received, wait until link is ready for data
                if(link_rdy = '1') then
                    next_state <= dma_write_data_fis;
                else
                    next_state <= dma_write_data_idle;
                end if;
            when dma_write_data_fis =>  --Need to make sure rdreq doesnt stay high and clock out all the data here
                if(pause = '0')then
                    next_state <= dma_write_data_frame;
                else
                    next_state <= dma_write_data_fis;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_write_data_frame   =>
                if(pause = '1')then
                    --paused_state <= dma_write_data_frame;
                    next_state <= pause_data_tx;
                elsif(tx_empty(tx_index) = '0') then
                    next_state <= dma_write_data_frame;
                else
                    next_state <= dma_write_chk_status;
                end if;
            when dma_write_chk_status   =>
                if(link_fis_type = REG_DEVICE_TO_HOST) then --link_fis_rdy = '1' and rx_data_in (7 downto 0)
                    --check error bit and device fault bit in the  Status field.. if error is asserted can check error field
                    --TODO: create constants for ERROR, DEV_FAULT, etc
                    --if(rx_data_in(STATUS_ERR) = '1' or rx_data_in(STATUS_DF) = '1') then
                    --error occured, update this part!
                    --  next_state <= transport_idle;
                    --elsif(rx_data_in(STATUS_BSY) = '0') then
                        next_state <= transport_idle;   --Go back to transport idle until error functionality added
                    --end if;
                else
                    next_state <= dma_write_chk_status;
                end if;
            when pause_data_tx =>
                if(pause = '0')then
                    next_state <= dma_write_data_frame;
                else
                    next_state <= pause_data_tx;
                end if;
--========================================================================================
            -- DMA Read EXT SM states
            --CHANGELOG:
            --Updated fis tx states to check pause flag
            --
            --TODO:
            when dma_read_idle      =>
                if (link_rdy = '1' and pause = '0') then --Should pause check be here? depends on link timing
                    next_state <= dma_read_reg_fis_0;
                else
                    next_state <= dma_read_idle;
                end if;
            when dma_read_reg_fis_0 =>
                if(pause = '0') then
                    next_state <= dma_read_reg_fis_1;
                else
                    next_state <= dma_read_reg_fis_0;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_read_reg_fis_1 =>
                if(pause = '0') then
                    next_state <= dma_read_reg_fis_2;
                else
                    next_state <= dma_read_reg_fis_1;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_read_reg_fis_2 =>
                if(pause = '0') then
                    next_state <= dma_read_reg_fis_3;
                else
                    next_state <= dma_read_reg_fis_2;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_read_reg_fis_3 =>
                if(pause = '0') then
                    next_state <= dma_read_reg_fis_4;
                else
                    next_state <= dma_read_reg_fis_3;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_read_reg_fis_4 =>
                if(pause = '0') then
                    next_state <= dma_read_data_fis;
                else
                    next_state <= dma_read_reg_fis_4;
                    --next_state <= pause_fis_tx;
                end if;
            when dma_read_data_fis  =>
            --add states to read entire fis
                if(link_fis_rdy = '1' and rx_data_in(7 downto 0)= DATA_FIS) then
                    next_state <= dma_read_data_frame;
                else
                    next_state <= dma_read_data_fis;
                end if;
            when dma_read_data_frame    =>
                --if(pause = '1')then
                --  paused_state <= dma_read_data_frame;
                --  next_state <= pause_data_rx;
                if(rx_full(rx_index) = '0') then --and data valid???
                    next_state <= dma_read_data_frame; --make sure this doesnt cause bugs because current_state is not updating??
                else
                    next_state <= dma_read_chk_status;
                end if;
            when dma_read_chk_status    =>  ----UPDATE THIS STATE
                if(link_fis_type = REG_DEVICE_TO_HOST) then --link_fis_rdy = '1' and rx_data_in (7 downto 0)
                    --check error bit and device fault bit in the  Status field.. if error is asserted can check error field
                    --TODO: create constants for ERROR, DEV_FAULT, etc
                    if(rx_data_in(STATUS_ERR) = '1' or rx_data_in(STATUS_DF) = '1') then
                    --error occured, update this part!
                        next_state <= transport_idle;
                    elsif(rx_data_in(STATUS_BSY) = '0') then
                        next_state <= transport_idle;   --Go back to transport idle until error state added
                    else
                        next_state <= dma_read_chk_status; --Maybe should include a timeout eventually
                    end if;
                else
                    next_state <= dma_read_chk_status;
                end if;
--=======================================================================================
            when others =>  next_state <= transport_idle;
        end case;
    end process;
--=================================================================================================================

    transport_output_logic: process(clk,rst_n) --(current_state, pause, link_status, link_rdy, link_fis_rdy, rx_data_in, link_fis_type, tx_full,
                                     --tx_almost_full, tx_almost_empty, tx_empty, tx_q, user_command,
                                     --rx_full, rx_almost_full, rx_empty, rx_almost_empty, tx_index, rx_index,
                                     --tx_fis_array, lba, paused_data_to_link)
      begin
        if(rst_n = '0')then
            rx_sclr(0) <= '1';
            rx_sclr(1) <= '1';
            rx0_locked <= '0';
            rx1_locked <= '0';
            rx_wrreq(0) <= '0';
            rx_wrreq(1) <= '0';

            tx0_locked <= '0';
            tx1_locked <= '0';
            tx_rdreq(0) <= '0';
            tx_rdreq(1) <= '0';

            --temporary signals for testing, may use later
            rx0_read_valid <= '0';
            rx1_read_valid <= '0';
            status_to_link(1 downto 0) <= "00";

        elsif(rising_edge(clk))then
        case (current_state) is
        ----------------------------------------------- -----------------------------------------------
            -- Idle SM states (top level)
        ----------------------------------------------- -----------------------------------------------
            when transport_reset =>
                --reset stuff
                rx_sclr(0) <= '1';
                rx_sclr(1) <= '1';
                rx0_locked <= '0';
                rx1_locked <= '0';
                rx_wrreq(0) <= '0';
                rx_wrreq(1) <= '0';

                tx0_locked <= '0';
                tx1_locked <= '0';
                tx_rdreq(0) <= '0';
                tx_rdreq(1) <= '0';

                --temporary signal for testing
                rx0_read_valid <= '0';
                rx1_read_valid <= '0';

                status_to_link(1 downto 0) <= "00"; --FOR TEST

            when transport_idle =>
                status_to_link(1 downto 0) <= "00";
                rx_sclr(0) <= '0';
                rx_sclr(1) <= '0';
                if (link_status = x"11111111") then --FIS RECEIVED

                elsif (tx_full(0) = '1') then   --User is sending "Write" command --Don't transition to DMA Write until a buffer is full
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
                elsif (user_command(1 downto 0) = "10") then
                    --lock rx buffer
                    --rx_locked <= '1';
                    --build read register FIS?
                else
                    --next_state <= transport_idle;
                end if;
    ----------------------------------------------- -----------------------------------------------

--========================================================================================
            -- DMA Write EXT SM states
            --TODO: check last states, create missing signals
                --tx_data_out, use tx_data_out or need intermediary signal?
            when dma_write_idle     =>
                --build register host to device DMA Write FIS
                tx_fis_array(tx_index).fis_type <= REG_HOST_TO_DEVICE;
                tx_fis_array(tx_index).crrr_pm <= x"80"; --80 sets C bit
                tx_fis_array(tx_index).command <= WRITE_DMA_EXT;
                tx_fis_array(tx_index).features <= x"00";
                tx_fis_array(tx_index).lba <= lba(23 downto 0);
                tx_fis_array(tx_index).device <= x"40";
                tx_fis_array(tx_index).features_ext <= x"00";
                tx_fis_array(tx_index).lba_ext <= lba(47 downto 24);
                tx_fis_array(tx_index).count <= WRITE_SECTOR_COUNT; --on most drives 1 logical sector := 512 bytes
                tx_fis_array(tx_index).icc <= x"00";
                tx_fis_array(tx_index).control <= x"00";
                tx_fis_array(tx_index).aux <= x"00000000";
            when dma_write_reg_fis_0    =>
                status_to_link(0) <= '1';
                tx_data_out <= tx_fis_array(tx_index).features & tx_fis_array(tx_index).command &
                               tx_fis_array(tx_index).crrr_pm & tx_fis_array(tx_index).fis_type;
            when dma_write_reg_fis_1    =>
                tx_data_out <= tx_fis_array(tx_index).device & tx_fis_array(tx_index).lba;
            when dma_write_reg_fis_2    =>
                tx_data_out <= tx_fis_array(tx_index).features_ext &
                               tx_fis_array(tx_index).lba_ext;
            when dma_write_reg_fis_3    =>
                tx_data_out <= tx_fis_array(tx_index).control & tx_fis_array(tx_index).icc &
                               tx_fis_array(tx_index).count;
            when dma_write_reg_fis_4    =>
                tx_data_out <= tx_fis_array(tx_index).aux;
                    --SET DEVICE BUSY BIT?

            when dma_write_chk_activate =>
                status_to_link(0) <= '0';--request to send data
                status_to_link(1) <= '1';--request to receive data
                --if(link_status = x"00000001") then --FIS RECEIVED
                --  if(link_fis_type = DMA_ACTIVATE_FIS) then
                --  elsif(error) then
                --      give up
                --  end if;
                --end if;
                tx_data_out <= x"00000000";
            when dma_write_data_idle => --Activate received, wait until link is ready for data
                status_to_link(0) <= '1';
                status_to_link(1) <= '0';
                tx_data_out <= x"00000000";
            when dma_write_data_fis =>
                tx_data_out <=  x"000000" & DATA_FIS;
                if(pause = '0')then
                    tx_rdreq(tx_index) <= '1';-- Need to update rdreq signal a clock cycle before data is desired
                else
                    tx_rdreq(tx_index) <= '0';-- Need to update rdreq signal a clock cycle before data is desired
                end if;
            when dma_write_data_frame   => --if puase and empty go high at the same time this will probably break!!!
                if(pause = '0') then
                    tx_data_out <= tx_q(tx_index);
                    if(tx_empty(tx_index) = '1') then
                        tx_rdreq(tx_index) <= '0';
                        status_to_link(0) <= '0';
                        if(tx_index = 0) then
                            tx0_locked <= '0';
                        else
                            tx1_locked <= '0';
                        end if;
                    end if;
                else --bring rdreq low and save last output data value
                    paused_data_to_link <= tx_q(tx_index);
                    tx_rdreq(tx_index) <= '0';
                end if;
            when dma_write_chk_status   =>  ----UPDATE THIS STATE
                status_to_link(1) <= '1';
                if(link_fis_rdy = '1' and rx_data_in (7 downto 0) = REG_DEVICE_TO_HOST) then
                    status_to_link(1) <= '0';
                    --check error bit and device fault bit in the  Status field.. if error is asserted can check error field
                    --TODO: create constants for ERROR, DEV_FAULT, etc
                    if(rx_data_in(STATUS_ERR) = '1' or rx_data_in(STATUS_DF) = '1') then
                        --error occured
                    elsif(rx_data_in(STATUS_BSY) = '0') then
                    else
                        --Loop here or go somewhere else?
                    end if;
                --elsif(error) then
                --  give up
                end if;
            when pause_data_tx =>
                if(pause = '0')then
                    tx_data_out <= paused_data_to_link;
                    tx_rdreq(tx_index) <= '1';
                else
                    tx_data_out <= x"FFFFFFFF"; --value for debugging
                end if;
--========================================================================================
            -- DMA Read EXT SM states
            --TODO: create state logic/assignments, check last states, create missing signals
            when dma_read_idle      =>
                status_to_link(0) <= '1';
                status_to_link(1) <= '0';
                --build register host to device DMA Read FIS
                rx_fis_array(rx_index).fis_type <= REG_HOST_TO_DEVICE;
                rx_fis_array(rx_index).crrr_pm <= x"00";
                rx_fis_array(rx_index).command <= READ_DMA_EXT;
                rx_fis_array(rx_index).features <= x"00";
                rx_fis_array(rx_index).lba <= lba(23 downto 0);
                rx_fis_array(rx_index).device <= x"40";
                rx_fis_array(rx_index).features_ext <= x"00";
                rx_fis_array(rx_index).lba_ext <= lba(47 downto 24);
                rx_fis_array(rx_index).count <= WRITE_SECTOR_COUNT; --on most drives 1 logical sector := 512 bytes
                rx_fis_array(rx_index).icc <= x"00";
                rx_fis_array(rx_index).control <= x"00";
                rx_fis_array(rx_index).aux <= x"00000000";

            when dma_read_reg_fis_0 =>
                tx_data_out <= rx_fis_array(rx_index).features & rx_fis_array(rx_index).command &
                               rx_fis_array(rx_index).crrr_pm & rx_fis_array(rx_index).fis_type;
            when dma_read_reg_fis_1 =>
                tx_data_out <= rx_fis_array(rx_index).device & rx_fis_array(rx_index).lba;
            when dma_read_reg_fis_2 =>
                tx_data_out <= rx_fis_array(rx_index).features_ext &
                               rx_fis_array(rx_index).lba_ext;
            when dma_read_reg_fis_3 =>
                tx_data_out <= rx_fis_array(rx_index).control & rx_fis_array(rx_index).icc &
                               rx_fis_array(rx_index).count;
            when dma_read_reg_fis_4 =>
                tx_data_out <= rx_fis_array(rx_index).aux;
                --SET DEVICE BUSY BIT?

            when dma_read_data_fis  => --pick a buffer
                status_to_link(0) <= '0';
                status_to_link(1) <= '1';
                if(rx_full(0) = '0') then
                    rx_index <= 0;
                    rx0_locked <= '1';
                elsif(rx_full(1) = '0') then
                    rx_index <= 1;
                    rx1_locked <= '1';
                end if;
            when dma_read_data_frame    => --store data into rx buffer
                if(pause = '0')then
                    if(rx_full(rx_index) = '0') then-- and data_valid = '1') then --data valid flag saying data from link is coming
                        rx_wrreq(rx_index) <= '1';
                        rx_data(rx_index) <= rx_data_in;
                    else
                        rx_wrreq(rx_index) <= '0';
                        if(rx_index = 0) then
                            rx0_locked <= '0';
                        else
                            rx1_locked <= '0';
                        end if;
                    end if;
                else
                    rx_wrreq(rx_index) <= '0';
                end if;
            when dma_read_chk_status    =>  ----UPDATE THIS STATE
                status_to_link(1) <= '1';
                if(link_fis_rdy = '1' and rx_data_in (7 downto 0) = REG_DEVICE_TO_HOST) then
                    status_to_link(1) <= '0';
                    --check error bit and device fault bit in the  Status field.. if error is asserted can check error field
                    --TODO: create constants for ERROR, DEV_FAULT, etc
                    if(rx_data_in(STATUS_ERR) = '1' or rx_data_in(STATUS_DF) = '1') then
                        --error occured
                    elsif(rx_data_in(STATUS_BSY) = '0') then
                    else
                        --Loop here or go somewhere else?
                    end if;
                --elsif(error) then
                --  give up
                end if;
            --when pause_data_rx => --SHouldn't actually need this state
            --  if(pause = '0')then
                    --rx_rdreq(rx_index) <= '1';
                    --rx_data(rx_index) <= paused_data;
            --  else
            --  end if;
--========================================================================================
            when others => -- state <= transport_idle;
        end case;
        end if;
    end process;
--=================================================================================================================
--Processes to control the flow of user data to/from the tx/rx buffers
--The dual-buffer system allows user data to be written to a buffer even when the Transort FSM is performing a command
    tx_buffer_control   : process(clk,rst_n)
      begin
        if(rst_n = '0') then
            tx_sclr(0) <= '1';  --clear buffer 1
            tx_sclr(1) <= '1';  --clear buffer 2
            tx_wrreq(0) <= '0';
            tx_wrreq(1) <= '0';
            --temporary signal for testing
            tx0_read_valid <= '0';
            tx1_read_valid <= '0';
            tx_write_ptr <= 0;
        elsif(rising_edge(clk)) then
            tx_sclr(0) <= '0';
            tx_sclr(1) <= '0';
            if(user_command(1 downto 0) = "01") then --user is sending data
                lba <= x"0000" &  write_address; --
                if(tx_almost_full(0) = '0' and tx0_locked = '0') then
                    --add user data to buffer
                    tx_wrreq(0) <= '1';
                    tx0_read_valid <= '0';
                    tx1_read_valid <= '1';
                    tx_data(0)  <= write_data;
                    tx_wrreq(1) <= '0';
                    tx_buffer(0)(tx_write_ptr) <= write_data; --buffer array
                    if(tx_write_ptr < BUFFER_DEPTH - 1)then
                        tx_write_ptr <= tx_write_ptr + 1;
                    else
                        tx_write_ptr <= 0;
                    end if;
                elsif(tx_almost_full(1) = '0' and tx1_locked = '0') then
                    --add user data to buffer
                    --temporary signal for testing
                    tx_wrreq(1) <= '1';
                    tx1_read_valid <= '0';
                    tx0_read_valid <= '1';
                    tx_data(1) <= write_data;
                    tx_wrreq(0) <= '0';
                    tx_buffer(1)(tx_write_ptr) <= write_data; --buffer array
                    if(tx_write_ptr < BUFFER_DEPTH - 1)then
                        tx_write_ptr <= tx_write_ptr + 1;
                    else
                        tx_write_ptr <= 0;
                    end if;
                else
                --ERROR, write invalid
                    tx_write_ptr <= 0;
                    tx_wrreq(0) <= '0';
                    tx_wrreq(1) <= '0';
                    tx0_read_valid <= '0';
                    tx1_read_valid <= '0';
                end if;
            else
                tx_wrreq(0) <= '0';
                tx_wrreq(1) <= '0';
            end if;
        end if;
    end process;

rx_buffer_control_reads : process(clk, rst_n)
      begin
        if(rst_n = '0') then
            rx_rdreq(0) <= '0';
            rx_rdreq(1) <= '0';
            rx_read_request(0) <= '0';
            rx_read_request(1) <= '0';
            read_data <= x"00000000";
            read_address <= x"00000000"; --Save input address for read address?
        elsif(rising_edge(clk)) then
            if(user_command(2) = '1') then
                if(rx0_locked = '0')then --and rx0_read_valid = '1') then
                    --immediately assert rdreq so value is valid on next clock cycle
                    --using rx_read_request to allow one clock cycle between bringing rdreq high and reading data
                    rx_rdreq(0) <= '1';
                    rx_read_request(0) <= '1';
                    if(rx_read_request(0) = '1') then
                        read_data <= rx_q(0);
                        if(rx_empty(0) = '1') then
                            rx_rdreq(0) <= '0';
                            rx_read_request(0) <= '0';
                        end if;
                    end if;
                elsif(rx1_locked = '0')then --and rx1_read_valid = '1') then --if(rx_empty(1) = '0'
                    rx_rdreq(1) <= '1';
                    rx_read_request(1) <= '1';
                    if(rx_read_request(1) = '1') then
                        read_data <= rx_q(1);
                        if(rx_empty(1) = '1') then
                            rx_rdreq(1) <= '0';
                            rx_read_request(1) <= '0';
                        end if;
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
--signal assignments for readibility

    link_fis_type <= rx_data_in(7 downto 0);
--============================================================================
--============================================================================
    --update status vectors
    status_to_user(0) <= '1'; --having device always be ready for now

    update_status : process(current_state, tx_full, rx_full, rx_empty, rx0_locked, rx1_locked)
        begin
        if ((tx_full(0) = '0' or tx_full(1) = '0') and current_state = transport_idle) then
            status_to_user(1) <= '1';
        else
                status_to_user(1) <= '0';
        end if;

        if ((rx_full(0) = '0' or rx_full(1) = '0') and current_state = transport_idle) then
            status_to_user(2) <= '1';
        else
                status_to_user(2) <= '0';
        end if;

        if ((rx_empty(0) = '0' and rx0_locked = '0') or (rx_empty(1) = '0' and rx1_locked = '0')) then
            status_to_user(3) <= '1';
        else
                status_to_user(3) <= '0';
        end if;
    end process;

    link_fis_rdy <= link_status(1);
    link_rdy <= link_status (5);
    pause <= link_status(6);--update

    status_to_link(7 downto 2) <= "000000"; --REMOVE THIS LATER
end architecture;