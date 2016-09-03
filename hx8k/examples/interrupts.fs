
\ An example for using the ticks counter overflow interrupt on Mecrisp-Ice

\ Interrupt frequency = 36 MHz / 2^16 = 549.31 Hz.

0 variable seconds
0 variable prescaler

: interrupt ( -- )
  1 prescaler +!
  prescaler @ 549 u>= 
  if 
    0 prescaler !
    1 seconds +!
    1 seconds @ 7 and lshift leds
  then
;

' interrupt 1 rshift $3FFE ! \ Generate JMP opcode for vector location
eint

: s seconds @ u. ;
