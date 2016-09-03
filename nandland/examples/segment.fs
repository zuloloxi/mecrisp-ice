
\ A small 7 segment display example

binary

create font
0111111 , \ 0
0000110 , \ 1
1011011 , \ 2
1001111 , \ 3
1100110 , \ 4
1101101 , \ 5
1111101 , \ 6
0000111 , \ 7
1111111 , \ 8
1101111 , \ 9
1110111 , \ A
1111100 , \ B
0111001 , \ C
1011110 , \ D
1111001 , \ E
1110001 , \ F

decimal

: >seg  ( u -- x ) 2* font + @ ;

: seg.x ( c -- )
  dup
  $F and >seg 7 lshift
  swap
  4 rshift $F and >seg
  or $80 io!
;

: ascii ( -- )
  begin
    key dup seg.x
    27 =
  until
;
