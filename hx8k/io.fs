
\  ------------------------------------------------------------
\    Useful Low-Level IO definitions
\  ------------------------------------------------------------
\
\     Addr  Bit READ            WRITE
\
\     0001  0   PMOD in
\     0002  1   PMOD out        PMOD out
\     0004  2   PMOD dir        PMOD dir
\     0008  3   misc.out        misc.out
\
\     0010  4   header 1 in
\     0020  5   header 1 out    header 1 out
\     0040  6   header 1 dir    header 1 dir
\     0080  7
\
\     0100  8   header 2 in
\     0200  9   header 2 out    header 2 out
\     0400  10  header 2 dir    header 2 dir
\     0800  11
\
\     1000  12  UART RX         UART TX
\     2000  13  misc.in
\     4000  14  ticks           clear ticks
\     8000  15   ----- SB_WARMBOOT -----
\
\
\ Contents of misc.out and misc.in:
\
\  Bitmask Bit  misc.out        misc.in
\
\    0001    0  SPI CS          UART Ready to Transmit
\    0002    1  SPI MOSI        UART Character received
\    0004    2  SPI SCK         SPI MISO
\    0008    3  IrDA-TXD        IrDA-RXD
\    0010    4  IrDA-Sleep      RTS
\    0020    5  CTS
\    0040    6  Green LED
\    0080    7  Red LED
\    0100    8  Red LED
\    0200    9  Red LED
\    0400   10  Red LED
\

: ms   ( u -- ) 0 do 1490 0 do loop loop ;
: leds ( x -- ) 6 lshift 8 io@ $3f and or 8 io! ;

: now   ( -- ) 0 $4000 io! ;
: ticks ( -- u ) $4000 io@ ;
: delay ( u -- ) begin dup ticks u< until drop ;
