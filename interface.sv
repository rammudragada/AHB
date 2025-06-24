interface ahb_intf;
parameter addr_width = 32;
parameter data_width = 32;
parameter burst_length = 4; 
parameter burst_type = 2; // 0: fixed, 1: incrementing, 2: wrapping
parameter hsel_width = 4; // Number of slaves

//AHB Global Signals
logic HCLK;
logic HRESETn;

//AHB Master Signals
logic HLOCK; // 0: not locked, 1: locked
logic HBUSREQ; // 0: no request, 1: request
logic [1:0] HTRANS; // 00: idle, 01: busy, 10: non-sequential, 11: sequential
logic [addr_width-1:0] HADDR;
logic HWRITE;
logic [2:0] HSIZE; // 0: byte, 1: half
logic [2:0] BURST; // 0: single, 1: incrementing, 2: wrapping, 3: reserved
logic [3:0] HPROT; // Protection type
logic [data_width-1:0] HWDATA;

//from arbiter
logic HGRANT; // 0: no grant, 1: grant

//from slave (bus)
logic HREADY; // 0: not ready, 1: ready
logic [1:0] HRESP; // 00: OKAY, 01: ERROR, 10: RETRY, 11: SPLIT
logic [data_width-1:0] HRDATA; // Read data from slave

modport ahb_master_mp (
    input  HCLK, HRESETn, HGRANT, HREADY, HRESP, HRDATA,
    output HLOCK, HBUSREQ, HTRANS, HADDR, HWRITE, HSIZE, BURST, HPROT, HWDATA
);
   
endinterface //ahb_intf

