
\ An example for using the ticks counter overflow interrupt on Mecrisp-Ice

\ Interrupt frequency = 48 MHz / 2^16 = 732.42 Hz.

0 variable seconds
0 variable prescaler

: interrupt ( -- )
  1 prescaler +!
  prescaler @ 732 u>= 
  if 
    0 prescaler !
    1 seconds +!
    2 seconds @ 3 and lshift leds
  then
;

' interrupt 1 rshift $1FFE ! \ Generate JMP opcode for vector location
eint

: s seconds @ u. ;
