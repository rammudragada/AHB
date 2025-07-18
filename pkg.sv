package ahb_pkg;
//AHB Master FSM States
// IDLE: Waiting for a transaction to start
// REQ: Requesting bus access
// WAIT: Waiting for bus grant
// ADDR: Sending address and control
// DATA: Sending/receiving data
// WAIT_RDY: Waiting for slave to be ready
// DEASSERT_BUS: Deasserting bus signals after transaction completion
typedef enum  logic[3:0] {IDLE,REQ,FIRST_ADDR,PIPELINE,DONE} fsm_state_t;
typedef enum logic[2:0] {SINGLE,INCR,WRAP} burst_mode_t;
typedef enum logic [1:0] 
{
    IDLE=2'b00, 
    BUSY=2'b01, 
    NONSEQ=2'b10, 
    SEQ=2'b11
} transfer_t;
typedef enum logic [2:0]
{
    SINGLE  =3'b000,
    INCR    =3'b001,
    WRAP4   =3'b010,
    INCR4   =3'b011,
    WRAP8   =3'b100,
    INCR8   =3'b101,
    WRAP16  =3'b110,
    INCR16  =3'b111
} burst_in;
typedef enum logic [1:0] {
  OKAY   = 2'b00,
  ERROR  = 2'b01,
  RETRY  = 2'b10,
  SPLIT  = 2'b11
} resp_t;
typedef enum logic [2:0] {
  NO_ERROR           = 3'b000,
  OVERFLOW_ERROR     = 3'b001,
  ABORT_BY_SLAVE     = 3'b010,
  SLAVE_RESPONSE_ERR = 3'b011,
  TIMEOUT_ERROR      = 3'b100
} error_code_t;
endpackage