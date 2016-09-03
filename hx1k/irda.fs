
new

\ -----------------------------------------------------------------------------
\ IrDA half-duplex communication
\ -----------------------------------------------------------------------------

\ For 9600 Baud:

1250 constant bitlength    \ = 12 MHz / 9600 Hz
 234 constant pulselength  \ = 12 Mhz / 9600 Hz * 3/16

\ -----------------------------------------------------------------------------

: irda-tx-bit ( x -- x* )
  dup $80 and
  if   now                                     bitlength delay
  else now 8 8 bis! pulselength delay 8 8 bic! bitlength delay
  then
  2*
;

: irda-tx ( c -- )
  false irda-tx-bit drop \ Start bit
  irda-tx-bit irda-tx-bit irda-tx-bit irda-tx-bit
  irda-tx-bit irda-tx-bit irda-tx-bit irda-tx-bit
  drop
  now bitlength 2* delay  \ "Transmit" two stop bits for proper timing.
;

\ -----------------------------------------------------------------------------

: ir? ( -- ? ) $2000 io@ 8 and 0= ;

: irda-rx-bit ( x -- x* )
  now
  2* \ Shift received bits
  0  \ No pulse seen yet
  begin
    ir? or
    bitlength ticks u<   \ Watch for pulses on the whole bitlength
  until
  invert if 1 or then
;

: (irda-rx) ( -- c )
  now bitlength 2/ delay   \ Delay half a bit time

  0
  irda-rx-bit irda-rx-bit irda-rx-bit irda-rx-bit
  irda-rx-bit irda-rx-bit irda-rx-bit irda-rx-bit

  \ Stop bit(s) = 1 --> no pulse(s). No need to wait.
;

: irda-rx ( -- c )
  begin ir? until \ Wait for the start bit pulse
  (irda-rx)
;

\ -----------------------------------------------------------------------------

: detect ( -- ) begin ir? leds key? until ;

: rx     ( -- ) begin ir? if (irda-rx) emit then key? until ;

: tx ( -- )
  'a'
  begin
    dup irda-tx
    dup 'z' = if drop 'a' else 1+ then
    10 ms
    key?
  until
  drop
;

: chat ( -- )
  cr
  begin
    ir?  if (irda-rx) emit then
    key? if key dup 27 = if drop exit then dup emit irda-tx then
  again
;
