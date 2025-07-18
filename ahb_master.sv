import ahb_pkg::*;
module apb_master 
#(  parameter hsize=3'b010  )
(
ahb_intf.ahb_master_mp ahb_bus;

// External signals from/to client of AHB master
input  logic         start_txn;    // Start transaction trigger
input  logic [31:0]  req_addr;     // Base address
input  logic [7:0]   txn_len;      // Transaction length in data words (32-bit) count starts from 0
input  logic         rw_mode;      // 0 = read, 1 = write
input  burst_in      burst;
input  logic [31:0]  wr_data;      
input  logic         wr_valid;
input  logic         wr_rdy;
output logic [31:0]  rd_data; 
output logic         rd_valid;
output logic         rd_rdy;
output logic         txn_done;

);

//Transaction tracking signals
logic [7:0] transferred_count; // Either read or write transfers completed
logic [7:0] total_transfers; // Either total read or write transfers
logic [31:0] lower_addr; // Lower address of the WRAP Burst transaction
logic [31:0] upper_addr; // Upper address of the WRAP Burst transaction
logic [31:0] curr_addr; // Current address being accessed in the burst transaction
logic [31:0] next_addr; // Next address to be accessed in the burst transaction
logic [1:0]  txn_done_reg; // Transaction done signal 
logic        txn_retry; // Transaction retry signal



// Internal registers to hold txn info from client
logic [31:0] req_addr_reg;
logic [7:0]  txn_len_reg; // Transaction length in beats
logic        rw_mode_reg; // Read/Write mode
burst_in     burst_reg;
logic        flush_rd;
logic        flush_wr;

logic [31:0]  wr_buffer [255:0]; // Data to be written in write burst
logic [31:0]  rd_buffer [255:0]; // Captured read burst data   

burst_mode_t burst_mode_reg;   // 0 = single, 1 = burst
fsm_state_t state, next_state;

logic [7:0] wr_count;
logic [7:0] rd_count;
/*
 * Read and Write buffers management
 */
// WRITE BUFFER
always_ff @(posedge ahb_bus.HCLK or negedge ahb_bus.HRESETn) begin
  if (!ahb_bus.HRESETn || flush_wr) begin
    wr_count <= 8'd0;
    wr_rdy   <= 1'b1;
  end else if (!start_txn) begin
    wr_count <= 8'd0;
    wr_rdy   <= 1'b1;
  end else if (wr_valid && wr_rdy && (wr_count <= txn_len_reg)) begin
    wr_buffer[wr_count] <= wr_data;
    wr_count <= wr_count + 1;
    if (wr_count + 1 == 256)
      wr_rdy <= 1'b0;  // Stop accepting more when full
  end
end

// READ BUFFER
always_ff @(posedge ahb_bus.HCLK or negedge ahb_bus.HRESETn) begin
  if (!ahb_bus.HRESETn || flush_rd) begin
    rd_count <= 8'd0;
    rd_data  <= 32'd0;
    rd_valid <= 1'b0;
  end else if (!start_txn) begin
    rd_count <= 8'd0;
    rd_data  <= 32'd0;
    rd_valid <= 1'b0;
  end else if (rd_rdy && (rd_count <= txn_len_reg)) begin
    if (rd_valid_count > rd_count)begin
    rd_data  <= rd_buffer[rd_count];
    rd_valid <= 1'b1;
    rd_count <= rd_count + 1;
    end
    else rd_valid <= 1'b0;
  end 
  else begin
    rd_valid <= 1'b0;
  end
end

// FSM next-state logic
always_comb begin
    next_state = state;
    case (state)
        IDLE: if (start_txn) next_state = REQ; else next_state = state;
        REQ:  if (ahb_bus.HGRANT) next_state = FIRST_ADDR; else next_state = state;
        FIRST_ADDR: if (ahb_bus.HREADY && ahb_bus.HRESP == OKAY) next_state = PIPELINE; else next_state = state;
        PIPELINE: begin
            if (txn_done_reg && !txn_retry) begin
                next_state = DONE; // Move to DONE state after transaction completion
            end else if (!txn_done_reg && txn_retry) begin
                next_state = REQ; // Retry transaction
            end else next_state = PIPELINE; // Continue in PIPELINE state
        end
        DONE: next_state = IDLE; // Reset to IDLE after completion
    endcase
end

always_comb begin
   case (burst_reg)
        SINGLE:begin burst_mode_reg  = SINGLE;  end
        INCR:  begin burst_mode_reg  = INCR;    end
        WRAP4: begin burst_mode_reg  = WRAP;   burst_len=4'b0011 end
        INCR4: begin burst_mode_reg  = INCR;   burst_len=4'b0011 end
        WRAP8: begin burst_mode_reg  = WRAP;   burst_len=4'b0111 end
        INCR8: begin burst_mode_reg  = INCR;   burst_len=4'b0111 end
        WRAP16:begin burst_mode_reg  = WRAP;   burst_len=4'b1111 end
        INCR16:begin burst_mode_reg  = INCR;   burst_len=4'b1111 end
    endcase
end
always_comb begin
    lower_addr = calc_lower_boundary(curr_addr,hsize,burst_reg);
    upper_addr = lower_addr+burst_len+1;
end
// FSM output logic
always_ff @(posedge ahb_bus.HCLK && negedge ahb_bus.HRESETn) begin 
        if (!ahb_bus.HRESETn)
        state <= IDLE;
    else
        state <= next_state;
end

always_ff @(posedge ahb_bus.HCLK or negedge ahb_bus.HRESETn) begin
    if (!ahb_bus.HRESETn) begin
        ahb_bus.HBUSREQ <= 1'b0;
        ahb_bus.HADDR   <= 32'b0;
        ahb_bus.HWRITE  <= 1'b0;
        ahb_bus.HTRANS  <= IDLE; // Idle state
        ahb_bus.HSIZE   <= 3'b010; // Default size (word)
        ahb_bus.BURST   <= SINGLE; // Default burst type
        ahb_bus.HPROT   <= 4'b0000; // Default protection type
        rd_data         <= 32'b0;
        rd_valid        <= 1'b0;
        txn_done        <= 1'b0;
    end 
    else begin
        case (state)
            IDLE: begin
                ahb_bus.HBUSREQ <= 1'b0; // Deassert bus request
            end
            REQ: begin
                ahb_bus.HBUSREQ <= 1'b1;  // request bus
                req_addr_reg    <= req_addr; // Store requested address
                curr_addr       <= req_addr;
                rw_mode_reg     <= rw_mode; // Store read/write mode
                txn_len_reg     <= txn_len; // Store transaction length
                burst_reg       <= burst; // Store burst length
            end
            FIRST_ADDR: begin
                transferred_count <= 8'd0; // Reset transferred count
                total_transfers  <= txn_len_reg; // Set total transfers based on transaction length
                case(burst_mode_reg)
                SINGLE:begin
                    if(ahb_bus.HREADY==1 && ahb_bus.HRESP==OKAY)
                    begin
                            ahb_bus.HADDR <= req_addr_reg; // Set address
                            ahb_bus.HWRITE <= rw_mode_reg; // Set read/write mode
                            ahb_bus.HTRANS <= NONSEQ; // Non-sequential transfer
                            ahb_bus.HSIZE <= hsize; // Word size
                            ahb_bus.BURST <= burst_reg; // Single burst
                            ahb_bus.HPROT <= 4'b0000; // Default protection type   
                            curr_addr <= req_addr_reg; // Initialize current address
                            next_addr <= 31'h0; // Initialize next address
                    end
                    else burst_mode_reg <= burst_mode_reg; 

                INCR: begin
                    if(ahb_bus.HREADY==1 && ahb_bus.HRESP==OKAY)
                    begin
                            ahb_bus.HADDR <= req_addr_reg; // Set address
                            ahb_bus.HWRITE <= rw_mode_reg; // Set read/write mode
                            ahb_bus.HTRANS <= NONSEQ; // Non-sequential transfer
                            ahb_bus.HSIZE <= hsize; // Word size
                            ahb_bus.BURST <= burst_reg; // Incremental burst
                            ahb_bus.HPROT <= 4'b0000; // Default protection type   
                            curr_addr   <= req_addr_reg;
                            next_addr   <=incr_next_addr(req_addr_reg,hsize);
                    end
                    else burst_mode_reg <= burst_mode_reg; 
                end
                WRAP: begin
                    if(ahb_bus.HREADY==1 && ahb_bus.HRESP==OKAY)
                    begin
                            ahb_bus.HADDR <= req_addr_reg; // Set address
                            ahb_bus.HWRITE <= rw_mode_reg; // Set read/write mode
                            ahb_bus.HTRANS <= NONSEQ; // Non-sequential transfer
                            ahb_bus.HSIZE <= hsize; // Word size
                            ahb_bus.BURST <= burst_reg; // Wrapping burst
                            ahb_bus.HPROT <= 4'b0000; // Default protection type   
                            curr_addr     <= req_addr_reg;
                            next_addr     <=wrap_next_addr(req_addr_reg,lower_addr,upper_addr,hsize);
                            
                    end
                    else burst_mode_reg <= burst_mode_reg; 
                endcase
            end
            PIPELINE:begin
                if(ahb_bus.HREADY)begin
                    case(ahb_bus.HRESP)
                        OKAY:begin
                                if(rw_mode_reg) begin
                                    ahb_bus.HWDATA <= wr_buffer[transferred_count];
                                    if(transferred_count==total_transfers)begin
                                        txn_done_reg<=1;
                                        txn_retry<=0;
                                    end
                                    else begin
                                        case(burst_mode_reg)
                                            SINGLE: begin 
                                                ahb_bus.HADDR <= curr_addr; // Set address
                                                ahb_bus.HWRITE <= rw_mode_reg; // Set read/write mode
                                                ahb_bus.HTRANS <= NONSEQ; // Non-sequential transfer
                                                ahb_bus.HSIZE <= hsize; // Word size
                                                ahb_bus.BURST <= burst_reg; // Single burst
                                                ahb_bus.HPROT <= 4'b0000; // Default protection type   
                                            end
                                            INCR: begin 
                                                ahb_bus.HADDR <= next_addr; // Increment address
                                                ahb_bus.HWRITE <= rw_mode_reg; // Set read/write mode
                                                ahb_bus.HTRANS <= SEQ; // Sequential transfer
                                                ahb_bus.HSIZE <= hsize; // Word size
                                                ahb_bus.BURST <= burst_reg; // Incremental burst
                                                ahb_bus.HPROT <= 4'b0000; // Default protection type   
                                                curr_addr <= next_addr; // Update current address
                                                next_addr <= incr_next_addr(next_addr,hsize);
                                                transferred_count<=transferred_count+1;
                                            end 
                                            WRAP: begin
                                                ahb_bus.HADDR <= next_addr; // Increment address
                                                ahb_bus.HWRITE <= rw_mode_reg; // Set read/write mode
                                                ahb_bus.HTRANS <= SEQ; // Sequential transfer
                                                ahb_bus.HSIZE <= hsize; // Word size
                                                ahb_bus.BURST <= burst_reg; // Incremental burst
                                                ahb_bus.HPROT <= 4'b0000; // Default protection type   
                                                curr_addr <= next_addr; // Update current address
                                                next_addr <= wrap_next_addr(next_addr,lower_addr,upper_addr,hsize);
                                                transferred_count<=transferred_count+1;
                                            end
                                        endcase
                                    end
                                end
                                else begin 
                                rd_buffer[transferred_count]<=ahb_bus.HRDATA;
                                rd_rdy <= 1'b1;
                                end
                            end
                        ERROR:begin

                        end
                    endcase
                end
            end
        endcase
    end
end
    
//lower boundary calculation for next addr
function automatic logic [31:0] calc_lower_boundary (
  input logic [31:0] addr,
  input logic [2:0]  hsize,
  input burst_in  hburst
);
  int burst_len_bytes;

  case (hburst)
    SINGLE: burst_len_bytes  = 1 << hsize;        // SINGLE
    WRAP4 : burst_len_bytes  = 4 << hsize;        // WRAP4
    WRAP8 : burst_len_bytes  = 8 << hsize;        // WRAP8
    WRAP16: burst_len_bytes  = 16 << hsize;       // WRAP16
    default: burst_len_bytes = 1 << hsize;       // fallback
  endcase

  calc_lower_boundary = addr & ~(burst_len_bytes - 1);
endfunction

//next addr calculation for INCR burst
function automatic logic [31:0] incr_next_addr (
    input logic [31:0] curr_addr,
    input logic [2:0]  hsize
);
    logic [31:0] beat_size;
begin
    // Compute beat size = 2^hsize (in bytes)
    beat_size = (1 << hsize);
    // Increment address by beat size
    incr_next_addr = curr_addr + beat_size;
end
endfunction

//next addr calculation for WRAP burst
function automatic logic [31:0] wrap_next_addr (
    input logic [31:0] curr_addr,
    input logic [31:0] lower_addr,
    input logic [31:0] upper_addr,
    input logic [2:0]  hsize
);
    logic [31:0] beat_size;
    logic [31:0] next;
begin
    beat_size = (1 << hsize);      // Beat size in bytes
    next = curr_addr + beat_size;  // Increment address

    // Check for wrap condition
    if (next >= upper_addr)
        wrap_next_addr = lower_addr; // Wrap back to lower boundary
    else
        wrap_next_addr = next;       // Continue incrementing
end
endfunction
endmodule