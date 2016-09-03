
new

\ Ledcomm for Icestick
\ Matthias Koch, 2015, GPL3

\ -----------------------------------------------------------------------------
\ Communication primitives
\ -----------------------------------------------------------------------------

: Outgoing   ( -- x true | false ) key? dup if key tuck emit then ;
: Incoming   ( x -- ) emit ;
: Connect    ( -- ) ."  (Up) "      1 leds ;
: Disconnect ( -- ) ."  (Down) " cr 0 leds ;

\ -----------------------------------------------------------------------------
\ Connect a clear red high-brightness LED with both pins to the Pmod-Connector
\ -----------------------------------------------------------------------------

 1 constant Anode
16 constant Kathode

\ Routines to drive the LED

: shine ( -- )  \ Let it shine
  Anode Kathode or 4 io!
  Anode            2 io!
;

: prepare-measurement ( -- )  \ Reverse charge junction capacitance
  Anode Kathode or 4 io!
        Kathode    2 io!

  3 0 do loop

  Anode            4 io!
;

: finish-measurement ( -- Flag )  \ Check for discharge due to photocurrent
  1 io@ Kathode and 0=
;

\ -----------------------------------------------------------------------------
\ Communication algorithm, see http://mecrisp.sourceforge.net/ledcomm.htm
\ -----------------------------------------------------------------------------

18 constant pulsesforsync \ Number of bits in a cell plus two

variable shinecounter
variable pulsesreceived
variable measurementcounter
variable lightpattern
variable outgoingbits
variable incomingbits

variable brightness ( Bright or dark ? )
true brightness !

: msb? ( x -- x Flag ) dup 0< ;

: createpulse ( -- )
  8 shinecounter ! \ Zero-pulse, also to be send when idling

  \ Wait for an amount of incoming pulses before consider the connection as up and running.
  \ Transmit long zero-pulses during synchronisation.

  pulsesreceived @ pulsesforsync =
  if

  outgoingbits @ ?dup if  \ Continue with current data
                      msb? if 4 shinecounter ! then \ Is a short one-pulse required ?
                      2*
                      dup 0= if 12 shinecounter ! then \ If this has been the marker bit, transmit a carry-pulse
                      outgoingbits !
                    else  \ Fetch and prepare new data bits
                      Outgoing
                      if
                        ?dup if
                               4 shinecounter ! \ Start transmitting with leading one

                               msb? if \ Is MSB already the leading one ?
                                      2* 1 or \ Rotate in a marker bit that will not be transmitted
                                    else
                                      2* 1 or \  Rotate in a marker bit that will not be transmitted
                                      begin
                                        msb?   \ Continue to shift until the leading one shifts out
                                        swap 2* swap
                                      until
                                    then

                               outgoingbits ! \ Store prepared bits for further transmission
                             else 12 shinecounter ! \ If data is zero, only transmit a carry-pulse.
                             then
                      then
                    then
  then
;

: recognizepulse ( -- )
  pulsesreceived @ pulsesforsync <>
  if \ Count incoming pulses to detect stable synchronisation
    1 pulsesreceived +!
    pulsesreceived @ pulsesforsync = if Connect then
  then

  lightpattern @
      %1111111111100 and    \ Detect carry-pulses with a duration of 11 to 14 base times
  dup %1111111111100 = if drop incomingbits @ Incoming
                             0 incomingbits ! \ Delete collection of incoming bits
                       else                    \  as data is transmitted with variable length

          %111111100 and    \ Detect zero-pulses with a duration of 7 to 10 base times
          %111111100 = if   incomingbits @ 2*      incomingbits !
                       else incomingbits @ 2* 1 or incomingbits !
                       then \ Detect one-pulses with a duration of 3 to 6 base times

                       then
;

: ledcomm-init ( -- )
  0 outgoingbits !
  pulsesreceived @ pulsesforsync = if Disconnect then
  0 pulsesreceived !

  brightness @ if
    \ For a bright Ledcomm node
    createpulse
    shine
  else
    \ For a dark Ledcomm node
    0 shinecounter !
    1 measurementcounter !
    prepare-measurement
  then
;

: ledcomm-tick ( -- )
  shinecounter @ ?dup
  if \ Shine counter not yet zero
    -1 shinecounter +!
    1- if
         shine
       else \ Shine counter just reached zero
         32 measurementcounter !
         0 lightpattern !
         prepare-measurement
       then
  else
    finish-measurement
    1 and lightpattern @ 2* or
    dup lightpattern !
    %11111 and
    %11100 = if
               recognizepulse
               createpulse
               shine
             else
               -1 measurementcounter +!
               measurementcounter @ if prepare-measurement
                               else ledcomm-init then
             then
  then
;

\ -----------------------------------------------------------------------------
\ For communication try "bright ledcomm" (default, shiny) or
\ "dark ledcomm" (dark mode, waiting for incoming connection).
\ At least one node needs to be in bright mode.
\ -----------------------------------------------------------------------------

: bright ( -- ) true brightness ! ;
: dark   ( -- ) false brightness ! ;

: ledcomm ( -- )
  ledcomm-init
  begin now ledcomm-tick 244 48 * delay ( 244 us @ 48 MHz inclusive algorithm ) again
;
