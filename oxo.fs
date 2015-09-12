\ oxo board

( 0 = free, 1 = x, -1 = o )
variable oxo-data 8 cells allot

( descending best squares on board. position if available, -1 if not )
variable best-squares 8 cells allot

( who was the last player? used to determine whose move it is )
variable last-player

( current move on board. 0 - 8 )
variable current-move

( how many moves, so far? used to determine draw. )
variable move-count

1 constant x
-1 constant o

: move-invalid?
   -1 = ;

: dup-move-invalid?
   dup move-invalid? ;

: empty? ( n -- empty? )
   0= ;

: x? ( n -- x? )
   x = ;

: o? ( n -- o? )
   o = ;

: first-play? ( -- first? )
   last-player @ 0= ;

: last-player-x? ( -- last-x? )
   last-player @ x? ;

: last-player-o? ( -- last-o? )
   last-player @ o? ;

: move-xy ( n -- x y )
   3 /mod ;

: xy-move ( x y -- n )
   3 * + ;

: current-move-xy ( -- x y )
   current-move @ move-xy ;

: current-move-x ( -- x )
   current-move-xy drop ;

: current-move-y ( -- y )
   current-move-xy nip ;

: oxo-element ( index -- address )
   cells oxo-data + ;

: free? ( n -- free? )
   oxo-element @ empty? ;

: oxo-element-xy ( x y -- address )
   xy-move oxo-element ;

: best-element ( index -- address )
   cells best-squares + ;

: initialise-best-squares ( -- )
   4 0 best-element !
   0 1 best-element !
   2 2 best-element !
   6 3 best-element !
   8 4 best-element !
   1 5 best-element !
   3 6 best-element !
   5 7 best-element !
   7 8 best-element ! ;

: find-best-square ( n -- index )
   9 0 do
     dup i best-element @ = if
       drop i leave
     then
   loop ;

: remove-best-square ( n -- )
   find-best-square -1 swap best-element ! ;

: win-check? ( v v v -- win? )
   dup >r =
   swap r> =
   and ;

: quick-win? ( p n1 n2 -- block? )
   oxo-element @
   swap
   oxo-element @
   win-check? ;

: block-required? ( n1 n2 -- block? )
   last-player @ -rot
   quick-win? ;

: find-blocking-move ( -- n/-1 )
   -1
   first-play? 0= if
   9 0 do
     i free? if
     i 0= if
       1 2 block-required?
       3 6 block-required?
       4 8 block-required?
       or or if drop i leave then
     then
     i 1 = if
       0 2 block-required?
       4 7 block-required?
       or if drop i leave then
     then
     i 2 = if
       0 1 block-required?
       5 8 block-required?
       4 6 block-required?
       or or if drop i leave then
     then
     i 3 = if
       0 6 block-required?
       4 5 block-required?
       or if drop i leave then
     then
     i 4 = if
       1 7 block-required?
       3 5 block-required?
       0 8 block-required?
       6 2 block-required?
       or or or if drop i leave then
     then
     i 5 = if
       3 4 block-required?
       2 8 block-required?
       or if drop i leave then
     then
     i 6 = if
       0 3 block-required?
       7 8 block-required?
       4 2 block-required?
       or or if drop i leave then
     then
     i 7 = if
       1 4 block-required?
       6 8 block-required?
       or if drop i leave then
     then
     i 8 = if
       0 4 block-required?
       6 7 block-required?
       2 5 block-required?
       or or if drop i leave then
     then
     then
   loop
   then ;

: next-best-square ( -- n )
   find-blocking-move dup-move-invalid? if
     9 0 do
       i best-element @ dup-move-invalid? if
         drop
       else
         nip leave
       then
     loop
   then ;

: .next-best-square ( -- )
   next-best-square dup-move-invalid? if
     drop
   else
     1+ cr ." The next best square is: " . cr
   then ;

: .x-piece ( -- )
   [char] x emit ;

: .o-piece ( -- )
   [char] o emit ;

: .bar ( -- )
   [char] | emit ;

: .hyphen ( -- )
   [char] - emit ;

: .piece ( v -- )
   dup empty? if
     space drop
   else
     x? if
       .x-piece
     else
       .o-piece
     then
   then ;

: oxo-element? ( n -- )
   oxo-element @ .piece ;

: x-play-valid? ( -- valid-play-x? )
   first-play? last-player-o? or ;

: o-play-valid? ( -- valid-play-o? )
   first-play? last-player-x? or ;

: .hyphen-row ( -- )
   11 0 do .hyphen loop ;

: .oxo-board ( -- )
   cr
   9 0 do
     space i oxo-element?
     i 1+ 3 mod 0= if
       cr
       i 7 < if .hyphen-row cr then
     else
       space .bar
     then
   loop
   cr ;

: new-oxo-game ( -- )
   oxo-data 9 cells erase
   0 last-player !
   0 move-count !
   initialise-best-squares
   .oxo-board
   .next-best-square ;

: second-diagonal ( -- v2 v4 v6 )
   ( 2 4 6 )
   2 oxo-element @
   4 oxo-element @
   6 oxo-element @ ;

: first-diagonal ( -- v0 v4 v8 )
   ( 0 4 8 )
   0 oxo-element @
   4 oxo-element @
   8 oxo-element @ ;

: win-check-second-diagonal? ( -- win? )
   second-diagonal win-check? ;

: win-check-first-diagonal? ( -- win? )
   first-diagonal win-check? ;

: diagonal-win? ( -- win? )
   current-move 2 mod 0= if
     win-check-first-diagonal?
     win-check-second-diagonal?
     or
   else
     0
   then ;

: win-check-row? ( y -- win? )
   dup dup
   0 swap oxo-element-xy @ -rot
   1 swap oxo-element-xy @ -rot
   2 swap oxo-element-xy @
   win-check? ;

: win-check-column? ( x -- win? )
   dup dup
   0 oxo-element-xy @ -rot
   1 oxo-element-xy @ -rot
   2 oxo-element-xy @
   win-check? ;

: row-win? ( -- win? )
   current-move-y win-check-row? ;
   
: column-win? ( -- win? )
   current-move-x win-check-column? ;
   
: win? ( -- win? )
   row-win?
   column-win?
   diagonal-win?
   or or ;

: draw? ( -- draw? )
   move-count @ 1+ dup move-count !
   8 > ;

: .end-checks ( -- )
   win? if
     ." You won!" cr
   else
     draw? if
       ." It's a draw!" cr
     else
       .next-best-square cr
     then
   then ;

: player! ( index value -- )
   swap dup current-move !
   dup remove-best-square
   swap dup rot oxo-element !
   .oxo-board
   last-player !
   .end-checks
   cr ;

: x! ( n -- )
   x-play-valid? if
     1- dup free? if
       x player!
     else
       drop ." Square already played." cr
     then
   else
     drop ." Not your go." cr
   then ;

: auto-x! ( -- )
   next-best-square dup-move-invalid? if
     drop ." No more moves." cr
   else
     1+ x!
   then ;

: o! ( n -- )
   o-play-valid? if
     1- dup free? if
       o player!
     else
       drop ." Square already played." cr
     then
   else
     drop ." Not your go." cr
   then ;

: auto-o! ( -- )
   next-best-square dup-move-invalid? if
     drop ." No more moves." cr
   else
     1+ o!
   then ;

cr

new-oxo-game
