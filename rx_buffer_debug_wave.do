onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /transport_layer_tb/clk
add wave -noupdate /transport_layer_tb/rst_n
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/clock
add wave -noupdate -radix decimal /transport_layer_tb/dut/rx0_data_buffer/data
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/rdreq
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/sclr
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/wrreq
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/empty
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/full
add wave -noupdate -radix decimal /transport_layer_tb/dut/rx0_data_buffer/q
add wave -noupdate -radix decimal /transport_layer_tb/user_data_in
add wave -noupdate -radix decimal /transport_layer_tb/user_data_out
add wave -noupdate -radix decimal /transport_layer_tb/user_addr_in
add wave -noupdate -radix decimal /transport_layer_tb/user_addr_out
add wave -noupdate /transport_layer_tb/status
add wave -noupdate /transport_layer_tb/command
add wave -noupdate /transport_layer_tb/link_status
add wave -noupdate -radix decimal /transport_layer_tb/data_to_link
add wave -noupdate -radix decimal /transport_layer_tb/data_from_link
add wave -noupdate -radix decimal /transport_layer_tb/fake_memory
add wave -noupdate /transport_layer_tb/dut/rst_n
add wave -noupdate /transport_layer_tb/dut/clk
add wave -noupdate -radix decimal /transport_layer_tb/dut/write_data
add wave -noupdate -radix decimal /transport_layer_tb/dut/write_address
add wave -noupdate /transport_layer_tb/dut/user_command
add wave -noupdate /transport_layer_tb/dut/status_to_user
add wave -noupdate -radix decimal /transport_layer_tb/dut/read_data
add wave -noupdate /transport_layer_tb/dut/link_status
add wave -noupdate -radix decimal /transport_layer_tb/dut/tx_data_out
add wave -noupdate -radix decimal /transport_layer_tb/dut/rx_data_in
add wave -noupdate /transport_layer_tb/dut/state
add wave -noupdate /transport_layer_tb/dut/tx_fis_array
add wave -noupdate /transport_layer_tb/dut/rx_fis_array
add wave -noupdate /transport_layer_tb/dut/tx0_locked
add wave -noupdate /transport_layer_tb/dut/tx1_locked
add wave -noupdate /transport_layer_tb/dut/rx0_locked
add wave -noupdate /transport_layer_tb/dut/rx1_locked
add wave -noupdate /transport_layer_tb/dut/tx_index
add wave -noupdate /transport_layer_tb/dut/rx_index
add wave -noupdate /transport_layer_tb/dut/tx_data
add wave -noupdate /transport_layer_tb/dut/tx_rdreq
add wave -noupdate /transport_layer_tb/dut/tx_sclr
add wave -noupdate /transport_layer_tb/dut/tx_wrreq
add wave -noupdate /transport_layer_tb/dut/tx_empty
add wave -noupdate /transport_layer_tb/dut/tx_full
add wave -noupdate /transport_layer_tb/dut/tx_q
add wave -noupdate -radix decimal /transport_layer_tb/dut/rx_data
add wave -noupdate /transport_layer_tb/dut/rx_rdreq
add wave -noupdate /transport_layer_tb/dut/rx_sclr
add wave -noupdate /transport_layer_tb/dut/rx_wrreq
add wave -noupdate /transport_layer_tb/dut/rx_empty
add wave -noupdate /transport_layer_tb/dut/rx_full
add wave -noupdate /transport_layer_tb/dut/rx_q
add wave -noupdate /transport_layer_tb/dut/tx_read_request
add wave -noupdate /transport_layer_tb/dut/rx_read_request
add wave -noupdate /transport_layer_tb/dut/tx0_read_valid
add wave -noupdate /transport_layer_tb/dut/tx1_read_valid
add wave -noupdate /transport_layer_tb/dut/rx0_read_valid
add wave -noupdate /transport_layer_tb/dut/rx1_read_valid
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/clock
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/data
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/rdreq
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/sclr
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/wrreq
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/empty
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/full
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/q
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/sub_wire0
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/sub_wire1
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/sub_wire2
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/data
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/clock
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/wrreq
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/rdreq
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/aclr
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/sclr
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/q
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/eccstatus
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/usedw
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/full
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/empty
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/almost_full
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/almost_empty
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_count_id
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_read_id
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_full_flag
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_empty_flag
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_almost_full_flag
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_almost_empty_flag
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_set_q_to_x
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_set_q_to_x_by_empty
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_tmp_q
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_eccstatus
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_write_id
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_write_latency1
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_write_latency2
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_write_latency3
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_wrt_count
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_empty_latency1
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_empty_latency2
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_data_ready
add wave -noupdate /transport_layer_tb/dut/tx0_data_buffer/scfifo_component/i_data_shown
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/sub_wire0
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/sub_wire1
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/sub_wire2
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/data
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/clock
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/wrreq
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/rdreq
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/aclr
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/sclr
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/q
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/eccstatus
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/usedw
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/full
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/empty
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/almost_full
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/almost_empty
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_count_id
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_read_id
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_full_flag
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_empty_flag
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_almost_full_flag
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_almost_empty_flag
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_set_q_to_x
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_set_q_to_x_by_empty
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_tmp_q
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_eccstatus
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_write_id
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_write_latency1
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_write_latency2
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_write_latency3
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_wrt_count
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_empty_latency1
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_empty_latency2
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_data_ready
add wave -noupdate /transport_layer_tb/dut/rx0_data_buffer/scfifo_component/i_data_shown
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {79500 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 281
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {74172 ps} {110227 ps}
