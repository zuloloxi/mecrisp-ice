`timescale 1 ns / 1 ps

`default_nettype none
`define WIDTH 16

module SB_RAM2048x2(
	output  [1:0] RDATA,
	input         RCLK, RCLKE, RE,
	input  [10:0] RADDR,
	input         WCLK, WCLKE, WE,
	input  [10:0] WADDR,
	input   [1:0] MASK, WDATA
);
	parameter INIT_0 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_1 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_2 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_3 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_4 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_5 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_6 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_7 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_8 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_9 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
	parameter INIT_F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

  wire [15:0] rd;

  SB_RAM40_4K #(
    .WRITE_MODE(3),
    .READ_MODE(3),
    .INIT_0(INIT_0),
    .INIT_1(INIT_1),
    .INIT_2(INIT_2),
    .INIT_3(INIT_3),
    .INIT_4(INIT_4),
    .INIT_5(INIT_5),
    .INIT_6(INIT_6),
    .INIT_7(INIT_7),
    .INIT_8(INIT_8),
    .INIT_9(INIT_9),
    .INIT_A(INIT_A),
    .INIT_B(INIT_B),
    .INIT_C(INIT_C),
    .INIT_D(INIT_D),
    .INIT_E(INIT_E),
    .INIT_F(INIT_F)
  ) _ram (
    .RDATA(rd),
    .RADDR(RADDR),
    .RCLK(RCLK), .RCLKE(RCLKE), .RE(RE),
    .WCLK(WCLK), .WCLKE(WCLKE), .WE(WE),
    .WADDR(WADDR),
    .MASK(16'h0000), .WDATA({4'b0, WDATA[1], 7'b0, WDATA[0], 3'b0}));

  assign RDATA[0] = rd[3];
  assign RDATA[1] = rd[11];

endmodule

module top(input oscillator,

           output D1, 
           output D2, 
           output D3, 
           output D4,
           
           input S1,
           input S2,
           input S3,
           input S4,           

           output TXD,        // UART TX
           input  RXD,        // UART RX

           output SCK,        // Flash SCK
           input  MISO,      // Flash MISO
           output MOSI,     // Flash MOSI
           output CS,      // Flash CS

           inout PORTA0,
           inout PORTA1,
           inout PORTA2,
           inout PORTA3,
           inout PORTA4,
           inout PORTA5,
           inout PORTA6,
           inout PORTA7,

           output Segment1A,
           output Segment1B,
           output Segment1C,
           output Segment1D,
           output Segment1E,
           output Segment1F,
           output Segment1G,
           output Segment2A,
           output Segment2B,
           output Segment2C,
           output Segment2D,
           output Segment2E,
           output Segment2F,
           output Segment2G,
           
           output VGA_HSync,
           output VGA_VSync,

           output VGA_Red_0,
           output VGA_Red_1,
           output VGA_Red_2,
           output VGA_Grn_0,
           output VGA_Grn_1,
           output VGA_Grn_2,
           output VGA_Blu_0,
           output VGA_Blu_1,
           output VGA_Blu_2
);

  // wire resetq = 1'b1;
  wire resetq = ~S4;

  wire clk = oscillator;

  wire io_rd, io_wr;
  wire [15:0] mem_addr;
  wire mem_wr;
  wire [15:0] dout;
  wire [15:0] io_din;
  wire [12:0] code_addr;

  reg unlocked = 0;

`include "../build/ram.v"

  reg interrupt = 0;

  // ######   PROCESSOR   #####################################

  j1 _j1(
    .clk(clk),
    .resetq(resetq),
    .io_rd(io_rd),
    .io_wr(io_wr),
    .mem_wr(mem_wr),
    .dout(dout),
    .io_din(io_din),
    .mem_addr(mem_addr),
    .code_addr(code_addr),
    .insn_from_memory(insn),
    .interrupt_request(interrupt)
  );

  // ######   TICKS   #########################################

  reg [15:0] ticks;

  wire [16:0] ticks_plus_1 = ticks + 1;

  always @(posedge clk)
    if (io_wr & mem_addr[14])
      ticks <= dout;
    else
      ticks <= ticks_plus_1;

  always @(posedge clk) // Generate interrupt on ticks overflow
    interrupt <= ticks_plus_1[16];

  // ######   PMOD   ##########################################

  reg [7:0] pmod_dir;   // 1:output, 0:input
  reg [7:0] pmod_out;
  wire [7:0] pmod_in;

  SB_IO #(.PIN_TYPE(6'b1010_01)) io0 (.PACKAGE_PIN(PORTA0), .D_OUT_0(pmod_out[0]), .D_IN_0(pmod_in[0]), .OUTPUT_ENABLE(pmod_dir[0]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io1 (.PACKAGE_PIN(PORTA1), .D_OUT_0(pmod_out[1]), .D_IN_0(pmod_in[1]), .OUTPUT_ENABLE(pmod_dir[1]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io2 (.PACKAGE_PIN(PORTA2), .D_OUT_0(pmod_out[2]), .D_IN_0(pmod_in[2]), .OUTPUT_ENABLE(pmod_dir[2]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io3 (.PACKAGE_PIN(PORTA3), .D_OUT_0(pmod_out[3]), .D_IN_0(pmod_in[3]), .OUTPUT_ENABLE(pmod_dir[3]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io4 (.PACKAGE_PIN(PORTA4), .D_OUT_0(pmod_out[4]), .D_IN_0(pmod_in[4]), .OUTPUT_ENABLE(pmod_dir[4]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io5 (.PACKAGE_PIN(PORTA5), .D_OUT_0(pmod_out[5]), .D_IN_0(pmod_in[5]), .OUTPUT_ENABLE(pmod_dir[5]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io6 (.PACKAGE_PIN(PORTA6), .D_OUT_0(pmod_out[6]), .D_IN_0(pmod_in[6]), .OUTPUT_ENABLE(pmod_dir[6]));
  SB_IO #(.PIN_TYPE(6'b1010_01)) io7 (.PACKAGE_PIN(PORTA7), .D_OUT_0(pmod_out[7]), .D_IN_0(pmod_in[7]), .OUTPUT_ENABLE(pmod_dir[7]));

  // ######   UART   ##########################################

  wire uart0_valid, uart0_busy;
  wire [7:0] uart0_data;
  wire uart0_wr = io_wr & mem_addr[12];
  wire uart0_rd = io_rd & mem_addr[12];
  wire UART0_RX;
  buart _uart0 (
     .clk(clk),
     .resetq(1'b1),
     .rx(RXD),
     .tx(TXD),
     .rd(uart0_rd),
     .wr(uart0_wr),
     .valid(uart0_valid),
     .busy(uart0_busy),
     .tx_data(dout[7:0]),
     .rx_data(uart0_data));

  // ######   LEDS & PIOS   ###################################

  reg [2:0] PIOS;
  assign {SCK, MOSI, CS} = PIOS;
  reg [3:0] LEDS;
  assign {D4,D3,D2,D1} = LEDS;

  reg [13:0] SEGMENTS;
  assign {Segment2G, Segment2F, Segment2E, Segment2D, Segment2C, Segment2B, Segment2A,
          Segment1G, Segment1F, Segment1E, Segment1D, Segment1C, Segment1B, Segment1A } = ~SEGMENTS;
          
  reg [10:0] VGA;
  assign { VGA_VSync, VGA_HSync,
           VGA_Blu_2, VGA_Blu_1, VGA_Blu_0,
           VGA_Grn_2, VGA_Grn_1, VGA_Grn_0,
           VGA_Red_2, VGA_Red_1, VGA_Red_0
         } = VGA;
  
  // ######   RING OSCILLATOR   ###############################

  wire [1:0] buffers_in, buffers_out;
  assign buffers_in = {buffers_out[0:0], ~buffers_out[1]};
  SB_LUT4 #(
          .LUT_INIT(16'd2)
  ) buffers [1:0] (
          .O(buffers_out),
          .I0(buffers_in),
          .I1(1'b0),
          .I2(1'b0),
          .I3(1'b0)
  );

  wire random = ~buffers_out[1];

  // ######   IO PORTS   ######################################

  /*        bit READ            WRITE

      0001  0   PMOD in
      0002  1   PMOD out        PMOD out
      0004  2   PMOD dir        PMOD dir
      0008  3   misc.out        misc.out

      0010  4                   VGA Colours
      0020  5                   VGA HSYNC
      0040  6                   VGA VSYNC
      0080  7   Segments        Segments

      0100  8 
      0200  9 
      0400  10
      0800  11

      1000  12  UART RX         UART TX
      2000  13  misc.in
      4000  14  ticks           clear ticks
      8000  15
  */

  assign io_din =

    (mem_addr[ 0] ? { 8'd0, pmod_in}                                                 : 16'd0) |
    (mem_addr[ 1] ? { 8'd0, pmod_out}                                                : 16'd0) |
    (mem_addr[ 2] ? { 8'd0, pmod_dir}                                                : 16'd0) |
    (mem_addr[ 3] ? { 9'd0, LEDS, PIOS}                                              : 16'd0) |

    (mem_addr[ 7] ? { 2'd0, SEGMENTS}                                                : 16'd0) |

    (mem_addr[12] ? { 8'd0, uart0_data}                                              : 16'd0) |
    (mem_addr[13] ? { 9'd0, S3, S2, S1, random, MISO, uart0_valid, !uart0_busy}      : 16'd0) |
    (mem_addr[14] ?         ticks                                                    : 16'd0) ;

  // Very few gates needed: Simply trigger warmboot by any IO access to $8000 / $8001 / $8002 / $8003.
  // SB_WARMBOOT _sb_warmboot ( .BOOT(io_wr & mem_addr[15]), .S1(mem_addr[1]), .S0(mem_addr[0]) );

  always @(posedge clk) begin

    if (io_wr & mem_addr[1])  pmod_out <= dout[7:0];
    if (io_wr & mem_addr[2])  pmod_dir <= dout[7:0];
    if (io_wr & mem_addr[3])  {LEDS, PIOS} <= dout[6:0];
    
    if (io_wr & mem_addr[4])  {VGA[8:0]}   <= dout[8:0];
    if (io_wr & mem_addr[5])  {VGA[  9]}   <= dout[  9];
    if (io_wr & mem_addr[6])  {VGA[ 10]}   <= dout[ 10];
    
    if (io_wr & mem_addr[7])  SEGMENTS <= dout[13:0];
  end

  // ######   MEMLOCK   #######################################

  // This is a workaround to protect memory contents during Reset.
  // Somehow it happens sometimes that the first memory location is corrupted during startup,
  // and as an IO write is one of the earliest things which are done, memory write access is unlocked
  // only after the processor is up and running and sending its welcome message.

  always @(negedge resetq or posedge clk)
  if (!resetq) unlocked <= 0;
  else         unlocked <= unlocked | io_wr;

endmodule // top
