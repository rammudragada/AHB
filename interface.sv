interface ahb_intf;
parameter addr_width = 32;
parameter data_width = 32;
parameter burst_length = 4; 
parameter burst_type = 2; // 0: fixed, 1: incrementing, 2: wrapping
parameter hsel_width = 4; // Number of slaves

/* 
 *  AHB Global Signals
 */
logic HCLK;
logic HRESETn;

/* 
 *  AHB Master Signals
 */

logic                   HLOCK; // 0: not locked, 1: locked
logic                   HBUSREQ; // 0: no request, 1: request
logic [addr_width-1:0]  HADDR;
logic                   HWRITE;//0 :read, 1: write
logic [2:0]             HSIZE; // 0: byte, 1: half
logic [3:0]             HPROT; // Protection type
logic [data_width-1:0]  HWDATA;
transfer_t              HTRANS; // 00: idle, 01: busy, 10: non-sequential, 11: sequential
burst_in                HBURST;// single,incrementing, wrapping

/*
 *  AHB Arbiter Signals
 */
logic                   HGRANT; // 0: no grant, 1: grant

/* 
 *AHB Slave Signals 
 */
logic                   HREADY; // 0: not ready, 1: ready
logic [data_width-1:0]  HRDATA; // Read data from slave
resp_t HRESP; // 00: okay, 01: error, 10: retry, 11: split


// AHB Master Modport
modport ahb_master_mp (
    input  HCLK, HRESETn, HGRANT, HREADY, HRESP, HRDATA,
    output HLOCK, HBUSREQ, HTRANS, HADDR, HWRITE, HSIZE, BURST, HPROT, HWDATA
);
   
endinterface 

