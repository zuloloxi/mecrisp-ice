
\ A small example to read the switches on Nandland Go board

: switches ( -- )
  begin
    $2000 io@ 4 rshift leds
  key? until  
;


