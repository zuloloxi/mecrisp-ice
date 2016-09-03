
\ An example for using the ticks counter overflow interrupt on Mecrisp-Ice

\ Interrupt frequency = 25 MHz / 2^16 = 381.47 Hz.

0 variable seconds
0 variable prescaler

: interrupt ( -- )
  1 prescaler +!
  prescaler @ 381 u>= 
  if 
    0 prescaler !
    1 seconds +!
    1 seconds @ 3 and lshift leds
  then
;

' interrupt 1 rshift $1FFE ! \ Generate JMP opcode for vector location
eint

: s seconds @ u. ;
