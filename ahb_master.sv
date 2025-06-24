//import ahb_pkg::*;
module apb_master 
#(
    parameter burst_length_t burst_len = SINGLE, // Burst type and its length type INCR,WRAP4,INCR4,WRAP8,INCR8,WRAP16,INCR16
    
)


(
ahb_intf.ahb_master_mp ahb_bus;

// External control from client
input  logic         start_txn;    // Start transaction trigger
input  logic         rw_mode;      // 0 = read, 1 = write
input  logic [3:0]   burst_len;    // # of beats in burst
input  logic [3:0] txn_len
input  logic [31:0]  req_addr;     // Base address
input  logic [31:0]  wr_buffer [0:15]; // Data to be written in write burst
output logic [31:0]  rd_buffer [0:15]; // Captured read burst data
output logic         txn_done;
);
// Internal registers to hold burst info
logic [3:0] burst_count;
logic [31:0] addr_reg;
logic is_write;
logic is_burst;
logic [3:0] txn_len_reg; // Transaction length in beats
fsm_state_t state, next_state;

always_ff @(posedge ahb_bus.HCLK && negedge ahb_bus.HRESETn) begin 
        if (!ahb_bus.HRESETn)
        state <= IDLE;
    else
        state <= next_state;
end

// FSM next-state logic
always_comb begin
    next_state = state;
    case (state)
        IDLE: if (start_txn) next_state = REQ;
        REQ:  if (ahb_bus.HGRANT) next_state = ADDR;
    endcase
end

// FSM output logic
always_ff @(posedge ahb_bus.HCLK or negedge ahb_bus.HRESETn) begin
    if (!ahb_bus.HRESETn) begin
        ahb_bus.HBUSREQ <= 1'b0;
        ahb_bus.HLOCK   <= 1'b0;
        // reset local latches
        burst_count     <= 4'd0;
        addr_reg        <= 32'd0;
        is_write        <= 1'b0;
        is_burst        <= 1'b0;
        txn_len_reg     <= 4'd0;
        txn_done        <= 1'b0;
    end else begin
        case (state)
            IDLE: begin
                ahb_bus.HBUSREQ <= 1'b0;
                if (start_txn) begin
                    // latch client inputs
                    burst_count <= burst_len;
                    addr_reg    <= req_addr;
                    is_write    <= rw_mode;
                    is_burst    <= burst_mode;
                    txn_len_reg <= txn_len;
                    txn_done    <= 1'b0;
                end
            end
            REQ: begin
                ahb_bus.HBUSREQ <= 1'b1;  // request bus
                ahb_bus.HLOCK   <= 1'b0;  // (optional lock logic here)
            end
            ADDR: begin
                
                ahb_bus.HADDR  <= addr_reg; // Set address
                ahb_bus.HWRITE <= is_write;  // Set write/read mode
                ahb_bus.HTRANS <= (txn_len_reg > 1) ? 2'b10 : 2'b01; // Non-sequential or busy
                ahb_bus.HSIZE  <= (is_write) ? 3'b010 : 3'b001; // Word or byte size
                ahb_bus.BURST  <= burst_len; // Set burst type
                ahb_bus.HPROT  <= 4'b0000; // Default protection type
            end
        endcase
    end
    
end







    
endmodule