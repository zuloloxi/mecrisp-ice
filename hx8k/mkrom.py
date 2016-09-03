code = list(open("build/iceimage.hex"))

template = """
  SB_RAM2048x2 #(
    .INIT_0(256'h{init[0]}),
    .INIT_1(256'h{init[1]}),
    .INIT_2(256'h{init[2]}),
    .INIT_3(256'h{init[3]}),
    .INIT_4(256'h{init[4]}),
    .INIT_5(256'h{init[5]}),
    .INIT_6(256'h{init[6]}),
    .INIT_7(256'h{init[7]}),
    .INIT_8(256'h{init[8]}),
    .INIT_9(256'h{init[9]}),
    .INIT_A(256'h{init[10]}),
    .INIT_B(256'h{init[11]}),
    .INIT_C(256'h{init[12]}),
    .INIT_D(256'h{init[13]}),
    .INIT_E(256'h{init[14]}),
    .INIT_F(256'h{init[15]})
  ) _{kind}{i} (
    .RDATA({din}[{hi}:{lo}]),
    .RADDR({raddr}),
    .RCLK(clk), .RCLKE(1'b1), .RE(1'b1),
    .WCLK(clk), .WCLKE(unlocked), .WE({we}),
    .WADDR({waddr}),
    .MASK(16'h0000), .WDATA({dout}[{hi}:{lo}]));
"""

def genram(hh, d, **args):
    init = [0 for i in range(16)]
    for i in range(8):
        for j in range(16):
            v = 0
            for b in range(256):
                col = (2 * i) + ((b >> 3) & 1)
                row = (
                    (b >> 4) +
                    (16 * j) +
                    256 * (b & 7)
                )
                v |= ((d[row] >> col) & 1) << b
            init[j] = "%064x" % v
        hh.write(template.format(i = i, lo = 2*i, hi = 2*i + 1, init = init,
                                 **args))
code = [int(v,16) for v in code]
bn0 = code[0*2048:1*2048]
bn1 = code[1*2048:2*2048]
bn2 = code[2*2048:3*2048]
bn3 = code[3*2048:4*2048]

hh = open("build/ram.v", "w")
hh.write("""
    wire [15:0] insn0, insn1, insn2, insn3;
    wire [15:0] insn;
""")
genram(hh, bn0,
       kind = "bn0",
       raddr = "code_addr[10:0]",
       waddr = "mem_addr[11:1]",
       we = "mem_wr & !mem_addr[12] & !mem_addr[13]",
       din = "insn0",
       dout = "dout"
       )
genram(hh, bn1,
       kind = "bn1",
       raddr = "code_addr[10:0]",
       waddr = "mem_addr[11:1]",
       we = "mem_wr & mem_addr[12]  & !mem_addr[13]",
       din = "insn1",
       dout = "dout"
       )
genram(hh, bn2,
       kind = "bn2",
       raddr = "code_addr[10:0]",
       waddr = "mem_addr[11:1]",
       we = "mem_wr & !mem_addr[12] & mem_addr[13]",
       din = "insn2",
       dout = "dout"
       )
genram(hh, bn3,
       kind = "bn3",
       raddr = "code_addr[10:0]",
       waddr = "mem_addr[11:1]",
       we = "mem_wr & mem_addr[12]  & mem_addr[13]",
       din = "insn3",
       dout = "dout"
       )
hh.write("""
    reg c11, c12;
    always @(posedge clk)
    begin
      {c12, c11} <= {code_addr[12], code_addr[11]};
    end

    always @*
    begin
      casez ({c12, c11}) // Depending on memory address, select different RAM blocks.
      2'b00: insn = insn0;
      2'b01: insn = insn1;
      2'b10: insn = insn2;
      2'b11: insn = insn3;
      endcase
    end
""")
hh.close()
