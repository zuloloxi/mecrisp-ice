
\ Swapforth Nucleus by James Bowman enhanced with constant folding and inline optimisations by Matthias Koch


\ -----------------------------------------------------------------------------
\  Low level definitions that are different for HX1K and HX8K
\ -----------------------------------------------------------------------------

\ Execute needs to be the first definition in HX8K as its address is hardwired.

header execute : execute >r ;

header-2-foldable lshift   :noname     lshift   ;
header-2-foldable rshift   :noname     rshift   ;
header-2-foldable arshift  :noname     arshift  ;
header-1-foldable 1+       :noname     1+       ;
header-1-foldable negate   : negate    invert 1+ ;
header-1-foldable 1-       :noname     1-       ;
header @                   : @         h# 4000 or >r ;
header rdepth              :noname     rdepth   ;

\ -----------------------------------------------------------------------------
\  Low level definitions
\ -----------------------------------------------------------------------------

header-1-foldable 0=       : 0=        d# 0 = ;
header-1-foldable cell+    : cell+     d# 2 + ;

header-2-foldable <>       : <>        = invert ;
header-1-foldable 0<>      : 0<>       d# 0 <> ;
header-2-foldable >        : >         swap < ;
header-1-foldable 0<       : 0<        d# 0 < ;
header-1-foldable 0>       : 0>        d# 0 > ;
header-2-foldable u>       : u>        swap u< ;

header-1-foldable drop     : tdrop     drop     ;

header-0-foldable false    : false d# 0 ;
header-0-foldable true     : true  d# -1 ;
header-3-foldable rot      : rot   >r swap r> swap ;
header-3-foldable -rot     : -rot  swap >r swap r> ;
header-2-foldable tuck     : tuck  swap over ;
header-2-foldable 2drop    : 2drop drop drop ;
header-1-foldable ?dup     : ?dup  dup if dup then ;

header-2-foldable 2dup     : 2dup  over over ;
header +!                  : +!    tuck @ + swap ! ;
header-4-foldable 2swap    : 2swap rot >r rot r> ;
header-4-foldable 2over    : 2over >r >r 2dup r> r> 2swap ;

header-2-foldable min      : min   2dup< if drop else nip then ;
header-2-foldable max      : max   2dup< if nip else drop then ;

header-2-foldable umin     : umin  2dupu< if drop else nip then ;
header-2-foldable umax     : umax  2dupu< if nip else drop then ;

header-2-foldable bounds   : bounds ( a n -- a+n a ) over+ swap ;

header-1-foldable abs      : abs       dup 0< if negate then ;

header-1-foldable cells    :noname     2*       ;
header-1-foldable 2*       :noname     2*       ;
header-1-foldable 2/       :noname     2/       ;
header !                   :noname     !        ;
header-2-foldable +        :noname     +        ;
header-2-foldable -        :noname     -        ;
header-2-foldable xor      :noname     xor      ;
header-2-foldable and      :noname     and      ;
header-2-foldable or       :noname     or       ;
header-1-foldable not      :noname     invert   ;
header-1-foldable invert   :noname     invert   ;
header-2-foldable =        :noname     =        ;
header-2-foldable <        :noname     <        ;
header-2-foldable u<       :noname     u<       ;
header-2-foldable swap     :noname     swap     ;
header-1-foldable dup      :noname     dup      ;
header-2-foldable over     :noname     over     ;
header-2-foldable nip      :noname     nip      ;

header io!      :noname     io!      ;
header io@      :noname     io@      ;
header depth    :noname     depth    ;

\ -----------------------------------------------------------------------------
\  Double numbers
\ -----------------------------------------------------------------------------

header-4-foldable d+
: d+                              ( augend . addend . -- sum . )
    swap >r + swap                  ( h al )
    r@ + swap                       ( l h )
    over r> u< -
;

header-2-foldable dnegate
: dnegate
    invert
    swap                        ( ~hi lo )
    negate                      ( ~hi -lo )
    tuck                        ( -lo ~hi -lo )
    0= -
;

header-4-foldable d-
: d-
  dnegate
  d+
;

header-2-foldable dabs
: dabs ( d -- ud )
    dup 0< if dnegate then
;

header-1-foldable s>d
: s>d dup 0< ;

header-3-foldable m+
: m+
    s>d d+
;

header-2-foldable d0=
: d0=
    or 0=
;

header-2-foldable d2*
: d2*
    2* over d# 0 < d# 1 and + swap 2* swap
;

\ -----------------------------------------------------------------------------
\  Multiplication
\ -----------------------------------------------------------------------------

header-2-foldable um*
: um*  ( u1 u2 -- ud )
    2dupum*high
    >r
    um*low
    r>
;

header-2-foldable *
: * ( u1 u2 -- u )
    um*low
;

\ : ud* ( ud1 u -- ud2 ) \ ud2 is the product of ud1 and u
\     tuck * >r
\     um* r> +
\ ;

\ -----------------------------------------------------------------------------
\  Division
\ -----------------------------------------------------------------------------

\ see Hacker's Delight (2nd ed) 9-4 "Unsigned Long Division"

: divstep  \ ( y x z )
    DOUBLE DOUBLE
    >r
    dup 0< >r
    d2*
    dup r> or r@ u< invert if
        r@ -
        swap 1+ swap
    then
    r>
;

header-3-foldable um/mod
: um/mod \ ( ud u1 -- u2 u3 ) ( 6.1.2370 )
    divstep divstep divstep divstep
    drop swap
;

\ -----------------------------------------------------------------------------
\  Terminal IO
\ -----------------------------------------------------------------------------

header emit?
: emit?  ( -- ? )  d# 1 : uartstat h# 2000 io@ overand = ;

header key?
: key?   ( -- ? )  d# 2   uartstat ;

header key
: key
    begin
        key?
    until
;fallthru
: key>
    h# 1000 io@
;

header emit
: emit
    begin
        emit?
    until
    h# 1000 io!
;

: 2emit
    emit emit
;

\ -----------------------------------------------------------------------------
\  Byte memory access, emulated
\ -----------------------------------------------------------------------------

header c@
: c@
    dup @ swap
    d# 1 and if d# 8 rshift then
    d# 255 and
;

header c!
: c! ( u c-addr -- )
    dup>r d# 1 and if
        d# 8 lshift
        h# 00ff
    else
        h# 00ff and
        h# ff00
    then
    r@ @ and
    or r>
;fallthru
: _!    ( x a -- ) \ subroutine version of store
    !
;

\ -----------------------------------------------------------------------------
\  Strings
\ -----------------------------------------------------------------------------

header space
: space
    d# 32 emit
;

header cr
: cr
    d# 10 emit
;

header-0-foldable bl
: bl
    d# 32
;

: hex4
    dup d# 8 rshift
    DOUBLE
;fallthru
: hex2
    dup d# 4 rshift
    DOUBLE
    h# f and
    dup d# 10 < if
        [char] 0
    else
        d# 55
    then
    +
    emit
;

header .x2
: .x2 hex2 space ;

header .x
: .x4 hex4 space ;

header count
: count
    d# 1 over+ swap c@
;

header type
: type
    bounds
    begin
        2dupxor
    while
        count emit
    repeat
    2drop
;

header /string
: /string
    dup >r - swap r> + swap
;

: 1/string
    d# 1
    /string
;

\ -----------------------------------------------------------------------------
\  Core variables
\ -----------------------------------------------------------------------------

create base     $a ,
create forth    0 ,
create dp       0 ,         \ Data pointer, grows up
create lastword 0 ,
create thisxt   0 ,
create sourceC  0 , 0 ,
create >in      0 ,
create state    0 ,
create rO       0 ,
create leaves   0 ,
create init     0 ,
create constantfoldingpointer 0 ,
create foldability 0 ,
create fineforoptimisation 0 ,
create tib      #128 allot

header-0-foldable state :noname state ;
header-0-foldable base  :noname base ;
header-0-foldable >in   :noname >in  ;
header-0-foldable forth :noname forth ;
header-0-foldable init  :noname init ;
header-0-foldable tib   :noname tib ;
header here             : here  dp @i ;

\ -----------------------------------------------------------------------------
\  Dictionary handling
\ -----------------------------------------------------------------------------

\ Dictionary structure:
\ 2 bytes link and foldability:  13 bits link, 3 bytes foldability.
\ 1 byte name length, msb denotes immediate + n bytes name, aligned.
\ Executable code.


: dictionarycount count d# 127 and ;

: nextword
    @ 2/ 2/ h# 3FFE and \ Remove flags for foldability.
;

header words : words
    forth @i
    begin
        dup
    while
        dup cell+
        dictionarycount type
        space
        nextword
    repeat
    drop
;

header-1-foldable aligned
: aligned
    1+ 2/ 2*
;

: 3rd   >r over r> swap ;

: lower ( c1 -- c2 ) \ c2 is the lower-case of c1
    dup [char] A - d# 26 u<
    h# 20 and +
;

: i<> ( c1 c2 -- f ) \ case-insensitive difference
    2dupxor h# 1f and if
        drop ;
    then
    lower swap lower xor
;

: sameword ( c-addr u wp -- c-addr u wp flag )
    2dup cell+ c@ d# 127 and = if              \ lengths match?
        3rd 3rd 3rd                 \ 3dup
        d# 3 + >r                   \ R: word in dictionary
        bounds
        begin
            2dupxor
        while
            dup c@ r@ c@ i<> if
                2drop rdrop false ;
            then
            1+
            r> 1+ >r
        repeat
        2drop rdrop true
    else
        false
    then
;

: >xt
    cell+
    dictionarycount +
    aligned
;

header align
: align
    here
    aligned
    dp _!
;

header sfind ( c-addr len -- c-addr len 0 | a-addr flags ) \ Flags are -1 for normal words, 1 for immediate words.
: sfind
    forth @i
    begin
        dup
    while
        sameword
        if
            nip nip
            dup >xt
            swap        ( xt wp )
                        \ wp lsb 0 means non-immediate, return -1
                        \        1 means immediate,     return  1
                        
            dup @ d# 7 and foldability _!                \ Three LSBs of Link are flags for foldability                       
            cell+ @ d# 128 and if d# 1 else d# -1 then   \ MSB of name length is flag for immediate
            ;
        then
        nextword
    repeat
;

\ -----------------------------------------------------------------------------
\  Memory
\ -----------------------------------------------------------------------------

header fill
: fill ( c-addr u char -- ) ( 6.1.1540 )
  >r  bounds
  begin
    2dupxor
  while
    r@ over c! 1+
  repeat
  rdrop 2drop
;

header cmove
: cmove ( addr1 addr2 u -- )
    bounds rot >r
    begin
        2dupxor
    while
        r@ c@ over c!
        r> 1+ >r
        1+
    repeat
    rdrop 2drop
;

header cmove>
: cmove> \ ( addr1 addr2 u -- )
    begin
        dup
    while
        1- >r
        over r@ + c@
        over r@ + c!
        r>
    repeat
    drop 2drop
;

header 2@
: 2@ \ ( a -- lo hi )
    dup cell+ @ swap @
;

header 2!
: 2! \ ( lo hi a -- )
    tuck!
    cell+ _!
;

\ -----------------------------------------------------------------------------
\  Input buffer and parsing
\ -----------------------------------------------------------------------------

header source
: source
    sourceC 2@
;

: source! ( addr u -- ) \ set the source
    sourceC 2!
;

\ From Forth200x - public domain

: isspace? ( c -- f )
    h# 21 u< ;

: isnotspace? ( c -- f )
    isspace? 0= ;

: xt-skip   ( addr1 n1 xt -- addr2 n2 ) \ gforth
    \ skip all characters satisfying xt ( c -- f )
    >r
    BEGIN
        over c@ r@ execute
        overand
    WHILE
        1/string
    REPEAT
    r> drop ;

header parse-name
: parse-name ( "name" -- c-addr u )
    source >in @i /string
    ['] isspace? xt-skip over >r
    ['] isnotspace?
;fallthru
: _parse
    xt-skip ( end-word restlen r: start-word )
    2dup d# 1 min + source drop - >in _!
    drop r>
    tuck -
;

create scratch 0 ,

: isnotdelim
    scratch @i <>
;

header parse
: parse ( "ccc<char" -- c-addr u )
    scratch _!
    source >in @i /string
    over >r
    ['] isnotdelim
    _parse
;

\ -----------------------------------------------------------------------------
\  Compiler
\ -----------------------------------------------------------------------------

header allot
: tallot
    dp +!
;

header ,
: w,
    here _!
    d# 2 tallot
;

header c,
: c,
    here c!
    d# 1 tallot
;

: prev ( -- addr ) dp @i d# 2 - ;
: prev@xor prev @ xor ;
: isliteral ( prt -- f ) @ 0< ; \ Literal opcodes have MSB set

header compile,
: compile,

  \ Literal @ --> Address for High-Call + Execute saves cycles
  fineforoptimisation @i  \ Check if optimisation is allowable
  if
    ['] @ over=           \ Check if @ is to be compiled
    if ( call-target )
      prev isliteral      \ Check if the previous opcode has been a literal
      if
         drop
         h# 4000 prev@xor \ Add $4000 to address to make this a high call
         prev _!
         h# 4006 jmp w,   \ A call to execute, which is close to the beginning of the nucleus. We have no constant folding here, so do not use ['] and prepare the opcode.
      then
    then
  then

  \ Inline positive literals
  dup isliteral \ Points to a literal opcode ?
  if
    dup cell+ @ h# 608c = \ Which is directly followed with an exit ?
    if
      @ jmp w, \ Inline it !
    then
  then

  dup @ h# e080 and h# 6080 = \ ALU Opcode with ret bits set ?
  if  @ h# ff73 and \ Inline it with ret bits removed.
  else
    2/ h# 4000 or \ Compile classically as a simple call opcode
  then
  w,
;

\ -----------------------------------------------------------------------------
\  String literals
\ -----------------------------------------------------------------------------

header s,
: s,
    dup c,
    bounds
    begin
        2dupxor
    while
        dup c@ c,
        1+
    repeat
    2drop
    align
;

: (sliteral)
    r>
    count
    2dup+ aligned
    >r
;

header-imm sliteral
:noname
    ['] (sliteral) compile,
    s,
;

\ -----------------------------------------------------------------------------
\  Flags and headers
\ -----------------------------------------------------------------------------

: mkheader
    align
    here lastword _!
    forth @i 2* 2* w,
    parse-name
    s,
    dp @i thisxt _!
;

header foldable
: foldable ( u -- )
  1+
  lastword @i  
: setbit ( bitmask addr -- )
  dup>r @ or r> _!
;

header immediate
:noname    
  d# 128                   \ Set MSB of name length
  lastword @i cell+
  setbit
;

\ -----------------------------------------------------------------------------
\  Compiler essentials
\ -----------------------------------------------------------------------------

header-imm [
: t[
    state
;fallthru
: off ( a -- ) d# 0 swap _! ;

header ]
: t]
    fineforoptimisation off  \  : --> No opcodes written yet - never recognize header bytes as opcodes !
    state                    \ ] --> Something strange might just went on. Careful !
;fallthru
: on  ( a -- ) true swap _! ;

header :
:noname
    mkheader t]
;

header :noname
:noname
    align dp @i
    dup thisxt _!
    lastword off
    t]
;

\ Do not handle ALU instructions for size reasons, as only negative literals and R-Stack cause ALUs to be compiled

header-imm exit
: texit
    fineforoptimisation @i
    if
      h# 4000 prev@xor
      h# e000 overand
      if
        drop
      else
        prev jmp _!
      then

      \ ALU + exit --> ALU with ret bits set
      prev @ h# f08c and h# 6000 = \ ALU opcode with neither ret nor return stack in opcode ?
      if
        h# 008C prev@xor prev jmp _!
      then

    then

    inline: exit
;

header-imm ;
:noname
    lastword @i ?dup if forth _! then
    texit
    t[
;

\ -----------------------------------------------------------------------------
\  Control structures
\ -----------------------------------------------------------------------------

\ Represent forward branches in one word
\ using the high-3 bits for the branch type,
\ and low 13 bits for the address.
\ THEN does the work of extracting the pieces.

header-imm ahead
: tahead
    here h# 0000 w,
;

header-imm if
: tif
    here h# 2000 w,
;

header-imm then     ( addr -- )
: tthen
    here 2/
    swap +!
;

header-imm begin
: tbegin
    dp @i 2/
;

header-imm again
: tagain
    w,
;

header-imm until
: tuntil
    h# 2000 or w,
;

header does>
:noname
    r> 2/
    lastword @i
    >xt cell+  \ ready to patch the RETURN
    _!
;

header-imm recurse
:noname
    thisxt @i compile,
;

\ -----------------------------------------------------------------------------
\  Counted loops
\ -----------------------------------------------------------------------------

\
\ How DO...LOOP is implemented
\
\ It uses two elements on the return stack:
\    index-limit is the counter; it starts negative and counts up. When it reaches 0, loop exits
\    limit is the offset. I can be computed from (index-limit+limit)
\
\ So DO receives ( limit start ) on the stack.
\
\ E.g. for "13 3 DO"
\      index-limit = -10
\      limit       = 13
\
\ So the loop runs:
\      index-limit      -10 -9 -8 -7 -6 -5 -4 -3 -2 -1
\      I                  3  4  5  6  7  8  9 10 11 12
\

: (do)  ( limit start -- )
    over -   ( limit start-limit )
    swap     ( start-limit limit )
    r>       ( start-limit limit ret )
    swap >r  ( start-limit ret )
    swap >r  ( ret )
    >r
;

: (?do)  ( -- ?  R: start-limit -- start-limit )
    r> r@ 0= swap >r
;

: (loopnext)  ( -- ?  R: limit index-limit -- limit index+1-limit )
    r>         ( addr )
    r>         ( addr index-lim )
    1+         ( addr index+1-lim )
    dup >r     ( addr index+1-lim )
    swap >r    ( index+1-lim )
    d# 0 =
;

: (+loopnext) ( inc -- ?  R: limit index-limit -- limit index+inc-limit )

    r> swap    ( addr inc )
    
    dup 0<                  \ Check sign of increment
    if                      \ Negative increment
      r@ +
      r> over u<        
    else                    \ Positive increment    
      r@ +
      r> overu>
    then

    swap >r    ( addr flag )
    swap >r    ( flag )
;

: (loopdone)  ( -- )
    r> rdrop rdrop >r
;

: do-common     \ common prefix for DO and ?DO
    leaves @i leaves off
    ['] (do) compile,
;

header-imm do
:noname
    do-common
: dotail
    tbegin
;

header-imm leave
: leave
: leave,
    dp @i
    leaves @i w,
    leaves _!
;

header-imm ?do
:noname
    do-common
    ['] (?do) compile,
    tif leave, tthen
    dotail
;

\ Finish compiling DO..LOOP
\ resolve each LEAVE by walking the chain starting at 'leaves'
\ compile a call to (loopdone)

: resolveleaves
    leaves @i
    begin
        dup
    while
        dup @ swap        ( next leaveptr )
        here 2/
        swap _!
    repeat
    drop
    leaves _!
    ['] (loopdone) compile,
;

header-imm loop
:noname
    ['] (loopnext) compile,
    tuntil
    resolveleaves
;

header-imm +loop
:noname
    ['] (+loopnext) compile,
    tuntil
    resolveleaves
;

header i  ( -- index  R: limit index-limit -- limit index-limit )
: i
  r>           ( addr )
  r>           ( addr index-lim )
  r@           ( addr index-lim lim )
  over+        ( addr index-lim index )
  swap >r
  swap >r
;

header j  ( -- indexb  R: limitb indexb-limitb limita indexa-limita -- limitb indexb-limitb limita indexa-limita )
: j

  r>           ( addr )
  r>           ( addr index-lima )
  r>           ( addr index-lima lima )
  
  r>           ( addr index-lima lima index-limb )
  r@           ( addr index-lima lima index-limb limb )
  over+        ( addr index-lima lima index-limb indexb )
  
  swap >r
  swap >r
  swap >r
  swap >r
;

header-imm unloop
:noname
    ['] (loopdone) compile,
;


\ -----------------------------------------------------------------------------
\  Number IO base
\ -----------------------------------------------------------------------------

header decimal
: decimal
    d# 10
;fallthru
: setbase
    base _!
;

header binary
: binary
  d# 2
  setbase
;

header hex
: hex
  d# 16
  setbase
;

\ -----------------------------------------------------------------------------
\  Return stack jugglers
\ -----------------------------------------------------------------------------

header-imm >r   :noname     inline: >r ;
header-imm r>   :noname     inline: r> ;
header-imm r@   :noname     inline: r@ ;
header-imm rdrop  :noname   inline: rdrop ;

\ -----------------------------------------------------------------------------
\  Compilation tools
\ -----------------------------------------------------------------------------

header abort
: abort
    [char] ? emit
    [ tdp @ dup insertquit ! 2 + org ]
;

header-imm literal
: tliteral
    dup 0< if
        invert tliteral
        inline: invert
    else
        h# 8000 or w,
    then
;

header-imm postpone
:noname
    parse-name sfind
    dup jz abort
    0< if
        tliteral
        ['] compile,
    then
    compile,
;

header '
:noname
    parse-name
    sfind
    jz abort
;

header char
:noname
    parse-name drop c@
;

header-imm-0-foldable \
:noname
    sourceC @i >in _!
;


\ -----------------------------------------------------------------------------
\  Number input
\ -----------------------------------------------------------------------------

: digit? ( c -- u f )
   lower
   dup h# 39 > h# 100 and +
   dup h# 160 > h# 127 and - h# 30 -
   dup base @i u<
;

create sign 0 ,
create doubleresult 0 ,

header number
: number ( addr len -- 0 )
         (             n 1 )
         (             d-low d-high 2 )


  d# 3 over= if
    over dup d# 2 + c@ [char] ' =
         swap       c@ [char] ' = and
         if
           drop 1+ c@ d# 1 exit
         then
  then


  doubleresult off
  sign off
  base @i >r

  d# 0 d# 0 2swap

  ( d-low d-high addr len )

  begin
    dup
  while
    >r dup>r c@

    [char] $ over= if drop hex             else
    [char] # over= if drop decimal         else
    [char] % over= if drop binary          else
    [char] - over= if drop sign on         else
    [char] . over= if drop doubleresult on else

     digit? if
              >r base @i
              tuck * >r um* r> +
              r> m+
            else
              drop 2drop rdrop rdrop
              r> base _! jmp false
            then

    then then then then then

    r> r> 1/string
  repeat
  2drop

  sign @i if dnegate then
  doubleresult @i if d# 2 else drop d# 1 then

  r> base _!
;

\ -----------------------------------------------------------------------------
\  Interpreter
\ -----------------------------------------------------------------------------

: flushconstants ( n*x count -- {n-1}*x count-1 ) \ Recursive to write them out in reverse order
  dup if \ Do nothing if count is zero
    dup d# 1 u> if 1- swap >r flushconstants r> swap then
    swap tliteral fineforoptimisation on
  then
;

: interpret
    begin
        parse-name
        dup
    while
      constantfoldingpointer @i true =  \ If it is not set, initialise constant folding pointer
      if depth d# 2 - constantfoldingpointer _! then \ Do not include the values of parse-name

      sfind \ Try to find the definition in dictionary
      ?dup
      if ( xt imm-flag ) \ Found.

        state @i
        if    \ Compile mode

          >r >r \ Move xt and imm-flag out of the way

          \ Calculate how many constants are available
          depth                       \ Calculate how many
          constantfoldingpointer @i -  \ constants are available now
          ( #constants R: xt imm-flag ) \ Number of constants available now on stack

          dup foldability @i 1- u< invert \ Check if there are enough constants left
          foldability @i 0<> \ Check if the current definition is foldable
          and
          if \ Fold it !
            drop
            r> r> drop execute
          else \ Not foldable or not enough constants available

            \ Write the constants into memory
            flushconstants drop

            \ No folding allowed over classic compilation
            constantfoldingpointer on

            r> r>

            \ Do classic compilation
            0< if compile, fineforoptimisation on else execute fineforoptimisation off then
          then

        else  \ Execute mode
          drop
          constantfoldingpointer on
          execute
        then

      else ( addr len ) \ Not found ? Perhaps it is a number.
        number jz abort \ Leave the literal(s) on the stack
      then

    repeat
    2drop
;

\ -----------------------------------------------------------------------------
\  Terminal input
\ -----------------------------------------------------------------------------

\ Unicode-friendly ACCEPT contibuted by Matthias Koch

: delchar ( addr len -- addr len )
    dup if d# 8 emit d# 32 emit d# 8 emit then

    begin
        dup 0= if exit then
        1- 2dup+ c@
        h# C0 and h# 80 <>
      until
;

header accept
: accept

    >r d# 0  ( addr len R: maxlen )

    begin
        key    ( addr len key R: maxlen )

        d# 9 over= if drop d# 32 then
        d# 127 over= if drop d# 8 then

        dup d# 31 u>
        if
            over r@ u<
            if
                dup emit
                >r 2dup+ r@ swap c! 1+ r>
            then
        then

        d# 8 over= if >r delchar r> then

        d# 10 over= swap d# 13 = or
    until

    rdrop nip
    space
;

header refill
: refill
    tib dup d# 128 accept
    source!
    true
;fallthru
: 0>in
    >in off
;

header evaluate
:noname
    source >r >r >in @i >r
    source! 0>in
    interpret
    r> >in _! r> r> source!
;


\ -----------------------------------------------------------------------------
\  Main loop of Forth
\ -----------------------------------------------------------------------------

header quit
: quit
    begin rdepth while rdrop repeat \ Clear the return stack
    begin depth while drop repeat \ Clear the stack
    cr
    decimal
    state off
    constantfoldingpointer on

    begin
        refill drop
        interpret
            space
            [char] k
            [char] o 2emit
            [char] . emit
            cr
    again
;

\ -----------------------------------------------------------------------------
\  SPI flash memory access
\ -----------------------------------------------------------------------------

\ Read-modify-write on IO

header bis! : bis! ( x addr -- ) dup>r io@             or  r> io! ;
header bic! : bic! ( x addr -- ) dup>r io@ swap invert and r> io! ;
header xor! : xor! ( x addr -- ) dup>r io@             xor r> io! ;

\ SPI Flash tools and loader

header idle
: idle
    d# 1 d# 8 io!
;

: spixbit
    dup 0< d# 2 and        \ extract MS bit
    dup d# 8 io!            \ lower SCK, update MOSI
    d# 4 + d# 8 io!          \ raise SCK
    2*                        \ next bit
    h# 2000 io@ d# 4 and +     \ read MISO, accumulate
;

header spix
: spix
    d# 8 lshift
    spixbit spixbit spixbit spixbit
    spixbit spixbit spixbit spixbit
    2/ 2/
;

header >spi
: >spi      spix drop ;

header spi>
: spi>      d# 0 spix ;


header load
: load ( sector -- )
  h# 03 >spi \ Read command
        >spi  \ Sector number
  h# 00 >spi   \ Address high
  h# 00 >spi    \ Address low

  spi> spi> d# 8 lshift or

  dup h# FFFF <> \ Execution starts at address 0, there always will be a valid opcode.
  if d# 0 !       \ $FFFF denotes an empty sector that should not be loaded.

    d# 2
    begin
      spi> spi> d# 8 lshift or over !
      d# 2 +
      dup h# 4000 =
    until

  then

  drop
  idle

  init @i ?dup if execute then \ The feshly loaded image might have init set

  quit
;


\ -----------------------------------------------------------------------------
\  Interrupts
\ -----------------------------------------------------------------------------

header eint : eint ( -- ) h# 0 h# 0080 io! ;
header dint : dint ( -- ) h# 0 h# 0800 io! ;

\ -----------------------------------------------------------------------------
\  Boot here
\ -----------------------------------------------------------------------------

: main
    dint     \ Disable interrupts
    key> drop \ Reset UART state

    cr
    [char] e
    [char] M 2emit
    [char] r
    [char] c 2emit
    [char] s
    [char] i 2emit
    [char] -
    [char] p 2emit
    [char] c
    [char] I 2emit
    bl
    [char] e 2emit
    [char] .
    [char] 0 2emit
    [char] 7 emit
    cr

    d# 3 load \ Try to load image from sector 3 if available. Bitstream is in sectors 0, 1, 2.
    \ quit
;

meta
    link @ t' forth tw!
    there  t' dp tw!
target
