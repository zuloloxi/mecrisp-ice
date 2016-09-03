\
\ This file contains definitions in high-level Forth for the
\ rest of SwapForth. Many words were already defined in
\ nucleus -- this file fills in the gaps.
\
\ This file is divided into sections for each word set in ANS
\ Forth.
\
\ The only definitions in this file should be specific to
\ J1a SwapForth.

\ #######   CORE AND DOUBLE   #################################

: [']
    ' postpone literal
; immediate 0 foldable

: [char]
    char postpone literal
; immediate 0 foldable

: (
    [char] ) parse 2drop
; immediate 0 foldable

: u>= ( u1 u2 -- ? ) u< invert ; 2 foldable
: u<= ( u1 u2 -- ? ) u> invert ; 2 foldable
: >=  ( n1 n2 -- ? )  < invert ; 2 foldable
: <=  ( n1 n2 -- ? )  > invert ; 2 foldable

: else
    postpone ahead
    swap
    postpone then
; immediate

: while
    postpone if
    swap
; immediate

: repeat
     postpone again
     postpone then
; immediate

: create
    :
    here 4 + postpone literal
    postpone ;
;

: >body
    @ 32767 and
;

: m*
    2dup xor >r
    abs swap abs um*
    r> 0< if dnegate then
; 2 foldable

include core.fs

: /mod      >r s>d r> sm/rem ; 2 foldable
: /         /mod nip ; 2 foldable
: mod       /mod drop ; 2 foldable

: ."
    [char] " parse
    state @ if
        postpone sliteral
        postpone type
    else
        type
    then
; immediate 0 foldable

\ #######   CORE EXT   ########################################

: unused
    $2000 here -
;

: pad
    here aligned
;

include core-ext.fs
include io.fs

\ Save memory image to SPI Flash

: waitspi ( -- )
  begin
    $70 >spi \ Read Flag status register
    spi> $80 and
    idle
  until
;

: spiwe ( -- )
    $06 >spi \ Write enable
    idle
;

: erase ( sector -- )
  ?dup if \ Never overwrite bitstream !
    spiwe
    $D8 >spi \ Sector erase
        >spi  \ Sector number
    $00 >spi   \ Address high
    $00 >spi    \ Address low
    idle
    waitspi
  then
;

: save ( sector -- )
  ?dup if \ Never overwrite bitstream !

    dup erase
    0      \ 8 kb in 256 byte pages
    begin
      spiwe

      $02 >spi \ Page program, 256 Bytes
     over >spi  \ Sector number
     dup
 8 rshift >spi   \ Address high
      $00 >spi    \ Address low

      begin
        dup c@ >spi
        1+
        dup $FF and 0=
      until

      idle
      waitspi

      dup $2000 =
    until
    2drop

  then \ Bitstream protection
;

: cornerstone
  create
    forth 2@        \ preserve FORTH and DP after this
    , 2 cells + ,
  does>
    2@ forth 2! \ restore FORTH and DP
;

cornerstone new

: dump
    ?dup
    if
        1- 4 rshift 1+
        0 do
            cr dup dup .x space space
            16 0 do
                dup c@ .x2 1+
            loop
            space swap
            16 0 do
                dup c@ dup bl 127 within invert if
                    drop [char] .
                then
                emit 1+
            loop
            drop
        loop
    then
    drop
;

#include insight.fs
