package ahb_pkg;
//AHB Master FSM States
// IDLE: Waiting for a transaction to start
// REQ: Requesting bus access
// WAIT: Waiting for bus grant
// ADDR: Sending address and control
// DATA: Sending/receiving data
// WAIT_RDY: Waiting for slave to be ready
// DEASSERT_BUS: Deasserting bus signals after transaction completion
typedef enum  logic[3:0] {IDLE,REQ,ADDR,DATA,DEASSERT_BUS} fsm_state_t;
typedef enum logic[2:0] {SINGLE,INCR,WRAP4,INCR4,WRAP8,INCR8,WRAP16,INCR16} burst_length_t;


typedef enum logic[1:0] {OKAY, ERROR, RETRY, SPLIT} hresp_t;

    
endpackage