
( Deep insight into dictionary and code )
( Matthias Koch )

: insight ( -- )  ( Long listing of everything inside of the dictionary structure )
    base @ hex cr
    forth @
    begin
        dup
    while
         ." Addr: "     dup .x
        ."  Link: "     dup @ -2 and .x
        ."  Flags: "    dup @ 1 and if ." I " else ." - " then
                        dup @ 13 rshift ?dup if 1- u. else ." - " then
        ."  Code: "     dup 2 + count + aligned .x
        space           dup 2 + count type
        @ $1FFE and cr
    repeat
    drop
    base !
;

: name. ( Address -- )  ( If the address is Code-Start of a dictionary word, it gets named. )

  dup $2000 and if ."   --> " $1FFF and .x ." @" exit then

  >r
  forth @
  begin
    dup
  while
    dup 2 + count + aligned ( Dictionary Codestart )
      r@ = if ."   --> " dup 2 + count type then
    @ $1FFE and
  repeat
  drop r> drop
;

: memstamp ( Addr -- ) dup .x ." : " @ .x ."   " ; ( Shows a memory location nicely )

variable disasm-$    ( Current position for disassembling )
variable disasm-cont ( Continue up to this position )

: disasm-step ( -- )
  disasm-$ @ memstamp
  disasm-$ @ @  ( Fetch next opcode )
  2 disasm-$ +! ( Increment position )

  dup $8000 and         if ." Imm  " $7FFF and                                         .x       exit then ( Immediate )
  dup $E000 and $0000 = if ." Jmp  " $1FFF and 2* dup                                  .x name. exit then ( Branch )
  dup $E000 and $2000 = if ." JZ   " $1FFF and 2* disasm-cont @ over max disasm-cont ! .x       exit then ( 0-Branch )
  dup $E000 and $4000 = if ." Call " $1FFF and 2* dup                                  .x name. exit then ( Call )
                           ." Alu  " $1FFF and    dup              .x       $80 and if ."       ret" then ( ALU )
;

: seec ( -- ) ( Continues to see )
  base @ hex cr
  0 disasm-cont !
  begin
    disasm-$ @ @
    dup  $E080 and $6080 =           ( Loop terminates with ret )
    swap $E000 and 0= or             ( or when an unconditional jump is reached. )
    disasm-$ @ disasm-cont @ u>= and ( Do not stop when there has been a conditional jump further )

    disasm-step cr
  until

  base !
;

: see ( -- ) ( Takes name of definition and shows its contents from beginning to first ret )
  ' disasm-$ !
  seec
;


