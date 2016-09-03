`default_nettype none

module j1(
  input wire clk,
  input wire resetq,

  output wire io_rd,
  output wire io_wr,
  output wire [15:0] mem_addr,
  output wire mem_wr,
  output wire [15:0] dout,

  input  wire [15:0] io_din,

  output wire [12:0] code_addr,
  input  wire [15:0] insn_from_memory,
  input  wire interrupt_request
);

  reg interrupt_enable = 0;  
  wire interrupt = interrupt_request & interrupt_enable;
  
  reg [4:0] dsp, dspN;          // Data stack pointer
  reg [15:0] st0, st0N;         // Top of data stack
  reg dstkW;                    // Data stack write

  reg [12:0] pc, pcN;           // Program Counter

  wire [15:0] insn = interrupt ? 16'h4FFF : insn_from_memory;  // Interrupt: Execute "Call 1FFE".
  wire [12:0] pc_plus_1 = interrupt ? pc : pc + 13'd1;         // Do not increment PC for interrupts to continue later at the same location.
  wire fetch = pc[12] & ~interrupt;                            // Memory fetch data on pc[12] only valid if this is no interrupt entry.

  reg rstkW;                    // Return stack write
  wire [15:0] rstkD;            // Return stack write value
  reg notreboot = 0;

  assign mem_addr = st0[15:0];
  assign code_addr = pcN;

  // The D and R stacks
  wire [15:0] st1, rst0;
  reg [1:0] dspI, rspI;

  stack2 #(.DEPTH(16)) rstack(.clk(clk), .rd(rst0), .we(rstkW), .wd(rstkD), .delta(rspI));
  stack2 #(.DEPTH(16)) dstack(.clk(clk), .rd(st1),  .we(dstkW), .wd(st0),   .delta(dspI));

  wire [16:0] minus = {1'b1, ~st0} + st1 + 1;

  wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];

  always @*
  begin
    // Compute the new value of st0
    casez ({fetch, insn[15:8]})
      9'b1_???_?????: st0N = insn_from_memory;                      // Memory fetch

      9'b0_1??_?????: st0N = { 1'b0, insn[14:0] };                  // Literal
      9'b0_000_?????: st0N = st0;                                   // Jump
      9'b0_010_?????: st0N = st0;                                   // Call
      9'b0_001_?????: st0N = st1;                                   // Conditional jump

      9'b0_011_?0000: st0N = st0;                                   // TOS
      9'b0_011_?0001: st0N = st1;                                   // NOS
      9'b0_011_?0010: st0N = st0 + st1;                             // +
      9'b0_011_?0011: st0N = st0 & st1;                             // and

      9'b0_011_?0100: st0N = st0 | st1;                             // or
      9'b0_011_?0101: st0N = st0 ^ st1;                             // xor
      9'b0_011_?0110: st0N = ~st0;                                  // invert
      9'b0_011_?0111: st0N = {16{(minus == 0)}};                    //  =

      9'b0_011_?1000: st0N = {16{signedless}};                      //  <
      9'b0_011_?1001: st0N = {st0[15], st0[15:1]};                  // 1 arshift
      9'b0_011_?1010: st0N = {st0[14:0], 1'b0};                     // 1 lshift
      9'b0_011_?1011: st0N = rst0;                                  // r@

      9'b0_011_?1100: st0N = minus[15:0];                           // -
      9'b0_011_?1101: st0N = io_din;                                // Read IO
      9'b0_011_?1110: st0N = {11'b0, dsp};                          // depth
      9'b0_011_?1111: st0N = {16{(minus[16])}};                     // u<

      default: st0N = {16{1'bx}};
    endcase
  end

  wire func_T_N =   (insn[6:4] == 1);
  wire func_T_R =   (insn[6:4] == 2);
  wire func_write = (insn[6:4] == 3);
  wire func_iow =   (insn[6:4] == 4);
  wire func_ior =   (insn[6:4] == 5);
  wire func_dint =  (insn[6:4] == 6);
  wire func_eint =  (insn[6:4] == 7);
    
  wire is_alu = !fetch & (insn[15:13] == 3'b011);

  assign mem_wr = notreboot & is_alu & func_write;
  assign io_wr  = notreboot & is_alu & func_iow;
  assign io_rd  = notreboot & is_alu & func_ior;
  assign dout   = st1;
  
  wire eint = notreboot & is_alu & func_eint;
  wire dint = notreboot & is_alu & func_dint;
  
  wire interrupt_enableN = (interrupt_enable | eint) & ~dint;

  // Value which could be written to return stack: Either return address in case of a call or TOS.
  assign rstkD = (insn[13] == 1'b0) ? {2'b0, pc_plus_1, interrupt_enable} : st0;

  always @*
  begin
    casez ({fetch, insn[15:13]})                          // Calculate new data stack pointer
    4'b1_???,
    4'b0_1??:   {dstkW, dspI} = {1'b1,      2'b01};          // Memory Fetch & Literal
    4'b0_001:   {dstkW, dspI} = {1'b0,      2'b11};          // Conditional jump
    4'b0_011:   {dstkW, dspI} = {func_T_N,  {insn[1:0]}};    // ALU
    default:    {dstkW, dspI} = {1'b0,      2'b00};          // Default: Unchanged
    endcase
    dspN = dsp + {dspI[1], dspI[1], dspI[1], dspI};

    casez ({fetch, insn[15:13]})                          // Calculate new return stack pointer
    4'b1_???:   {rstkW, rspI} = {1'b0,      2'b11};          // Memory Fetch, triggered by high address bit set
    4'b0_010:   {rstkW, rspI} = {1'b1,      2'b01};          // Call
    4'b0_011:   {rstkW, rspI} = {func_T_R,  insn[3:2]};      // ALU
    default:    {rstkW, rspI} = {1'b0,      2'b00};          // Default: Unchanged
    endcase

    casez ({notreboot, fetch, insn[15:13], insn[7], |st0})   // New address for PC
    7'b0_0_???_?_?:   pcN = 0;                               // Boot: Start at address zero
    7'b1_0_000_?_?,
    7'b1_0_010_?_?,
    7'b1_0_001_?_0:   pcN = insn[12:0];                      // Jumps & Calls: Destination address
    7'b1_1_???_?_?,
    7'b1_0_011_1_?:   pcN = rst0[13:1];                      // Memory Fetch & ALU+exit: Return
    default:          pcN = pc_plus_1;                       // Default: Increment PC to next opcode
    endcase
  end

  always @(negedge resetq or posedge clk)
  begin
    if (!resetq) begin
      notreboot <= 0;
      { pc, dsp, st0, interrupt_enable } <= 0;
    end else begin
      notreboot <= 1;
      { pc, dsp, st0, interrupt_enable}  <= { pcN, dspN, st0N, interrupt_enableN };
    end
  end

endmodule
