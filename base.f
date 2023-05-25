: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

: '\n' 10 ;
: BL 32 ;
: '\r' 13 ;

: CR '\n' EMIT ;
: LF '\r' EMIT ;
: NEWLINE CR LF ;
: SPACE BL EMIT ;

: TRUE 1 ;
: FALSE 0 ;
: NOT 0= ;

: NEGATE 0 SWAP - ;

: LITERAL IMMEDIATE
  ' LIT ,
  ,
  ;

: ':'
  [
  CHAR :
  ]
  LITERAL
;

: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '[' [ CHAR [ ] LITERAL ;
: ']' [ CHAR ] ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: 'B' [ CHAR B ] LITERAL ;
: 'C' [ CHAR C ] LITERAL ;
: 'D' [ CHAR D ] LITERAL ;
: 'E' [ CHAR E ] LITERAL ;
: 'F' [ CHAR F ] LITERAL ;
: 'G' [ CHAR G ] LITERAL ;
: 'H' [ CHAR H ] LITERAL ;
: 'I' [ CHAR I ] LITERAL ;
: 'J' [ CHAR J ] LITERAL ;
: 'K' [ CHAR K ] LITERAL ;
: 'L' [ CHAR L ] LITERAL ;
: 'M' [ CHAR M ] LITERAL ;
: 'N' [ CHAR N ] LITERAL ;
: 'O' [ CHAR O ] LITERAL ;
: 'P' [ CHAR P ] LITERAL ;
: 'Q' [ CHAR Q ] LITERAL ;
: 'R' [ CHAR R ] LITERAL ;
: 'S' [ CHAR S ] LITERAL ;
: 'T' [ CHAR T ] LITERAL ;
: 'U' [ CHAR U ] LITERAL ;
: 'V' [ CHAR V ] LITERAL ;
: 'W' [ CHAR W ] LITERAL ;
: 'X' [ CHAR X ] LITERAL ;
: 'Y' [ CHAR Y ] LITERAL ;
: 'Z' [ CHAR Z ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '1' [ CHAR 1 ] LITERAL ;
: '2' [ CHAR 2 ] LITERAL ;
: '3' [ CHAR 3 ] LITERAL ;
: '4' [ CHAR 4 ] LITERAL ;
: '5' [ CHAR 5 ] LITERAL ;
: '6' [ CHAR 6 ] LITERAL ;
: '7' [ CHAR 7 ] LITERAL ;
: '8' [ CHAR 8 ] LITERAL ;
: '9' [ CHAR 9 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

: CLRPAGE 27 EMIT '[' EMIT '2' EMIT 'J' EMIT NEWLINE ;
: CLRLINE 27 EMIT '[' EMIT '2' EMIT 'K' EMIT LF ;

: [COMPILE] IMMEDIATE
  WORD
  FIND
  >CFA
  ,
;

: RECURSE IMMEDIATE
  LATEST @
  >CFA
  ,
;

: IF IMMEDIATE
  ' 0BRANCH ,
  HERE @
  0 ,
;

: THEN IMMEDIATE
  DUP
  HERE @ SWAP -
  SWAP !
;

: ELSE IMMEDIATE
  ' BRANCH ,
  HERE @
  0 ,
  SWAP
  DUP
  HERE @ SWAP -
  SWAP !
;

: BEGIN IMMEDIATE
  HERE @
;

: UNTIL IMMEDIATE
  ' 0BRANCH ,
  HERE @ -
  ,
;

: AGAIN IMMEDIATE
  ' BRANCH ,
  HERE @ -
  ,
;

: WHILE IMMEDIATE
  ' 0BRANCH ,
  HERE @
  0 ,
;

: REPEAT IMMEDIATE
  ' BRANCH ,
  SWAP
  HERE @ - ,
  DUP
  HERE @ SWAP -
  SWAP !
;

: UNLESS IMMEDIATE
  ' NOT ,
  [COMPILE] IF
;

: ( IMMEDIATE
  1
  BEGIN
    KEY
    DUP '(' = IF
        DROP
        1+
    ELSE
        ')' = IF
            1-
        THEN
    THEN
  DUP 0= UNTIL
  DROP
;

: NIP ( x y -- y ) SWAP DROP ;
: TUCK ( x y -- y x y ) SWAP OVER ;
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
  1+
  8 *
  DSP@ +
  @
;
 
: SPACES ( n -- )
  BEGIN
    DUP 0>
  WHILE
    SPACE
    1-
  REPEAT
  DROP
;

( EXTRA: Writes N zeroes to stdout )
: ZEROES ( n -- )
    BEGIN
        DUP 0>
    WHILE
        '0' EMIT
        1-
    REPEAT
    DROP
;

( Standard words for manipulating BASE. )
: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- ) 16 BASE ! ;

: U.
  BASE @ /MOD
  ?DUP IF
    RECURSE
  THEN

  DUP 10 < IF
    '0'
  ELSE
    10 -
    'A'
  THEN
  +
  EMIT
;

: 8+
  8
  +
;

: 8-
  8
  -
;

: .S ( -- )
  DSP@
  BEGIN
    DUP S0 @ <
  WHILE
    DUP @ U.
    SPACE
    8+
  REPEAT
  DROP
;

: UWIDTH ( u -- )
  BASE @ / ( u base -- quot rem )
  ?DUP IF ( if quot is not zero, dup: quot rem -- quot quot )
    RECURSE 1+
  ELSE
    1
  THEN
;

: U.R ( u width -- )
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -
  SPACES
  U.
;

: ZU.R ( u width -- )
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -
  ZEROES
  U.
;

: .R ( n width -- )
  SWAP
  DUP 0< IF
    NEGATE
    1
    SWAP
    ROT
    1-
  ELSE
    0
    SWAP
    ROT
  THEN
  SWAP
  DUP
  UWIDTH
  ROT
  SWAP -

  SPACES
  SWAP

  IF '-' EMIT THEN
  U.
;

( finally we can define the pop print word )
: . 0 .R SPACE ;

: U. U. SPACE ;

: ? ( addr - ) @ . ;

: WITHIN
  -ROT
  OVER
  <= IF
    > IF
      TRUE
    ELSE
      FALSE
    THEN
  ELSE
    2DROP
    FALSE
  THEN
;

( returns the depth of the stack, in bytes )
: DEPTH
  S0 @ DSP@ -
  8-
;

: ALIGNED
  7 + 7 INVERT AND
;

: ALIGN HERE @ ALIGNED HERE ! ;

: C,
  HERE @ C!
  1 HERE +!
;

: S" IMMEDIATE
     STATE @ IF
       ' LITSTRING ,
       HERE @
       0 ,
       BEGIN
         KEY
         DUP '"' <>
       WHILE
         C,
       REPEAT
       DROP
       DUP
       HERE @ SWAP -
       8-
       SWAP !
       ALIGN
     ELSE
       HERE @
       BEGIN
         KEY
         DUP '"' <>
       WHILE
         OVER C!
         1+
       REPEAT
       DROP
       HERE @ -
       HERE @
       SWAP
     THEN
;

: ." IMMEDIATE
     STATE @ IF
       [COMPILE] S"
       ' TELL ,
     ELSE
       BEGIN
         KEY
         DUP '"' = IF
           DROP
           EXIT
         THEN
         EMIT
       AGAIN
     THEN
;

( Words to change whether we are reading by line or characters )
: USELINES CLRLINE 1 LINEMODE ! ." LINES ON" NEWLINE ;
: USECHARS CLRLINE 0 LINEMODE ! ." CHARS ON" NEWLINE ;

: CONSTANT
  WORD     ( get the name, follows constant )
  CREATE   ( make entry )
  DOCOL ,  ( append docol )
  ' LIT ,  ( append codeword of lit )
  ,        ( append value on top of stack )
  ' EXIT , ( append codeword of exit )
;

: ALLOT ( n -- addr )
  HERE @ SWAP ( here n )
  HERE +! ( adds n to HERE, old HERE still on stack )
;

: CELLS ( n -- n ) 8 * ;

( this works because the old value of here, that which allot now points to, is still on the stack )
: VARIABLE
  1 CELLS ALLOT ( get 1 cell of memory )
  WORD CREATE   ( create a word, name is after variable )
  DOCOL ,       ( append docol )
  ' LIT ,       ( append lit )
  ,             ( append pointer to new memory )
  ' EXIT ,
;

: VALUE ( n -- )
  WORD CREATE
  DOCOL ,
  ' LIT ,
  ,
  ' EXIT ,
;

: TO IMMEDIATE ( n -- )
     WORD
     FIND
     >DFA
     8+
     STATE @ IF ( compiling? )
       ' LIT ,
       ,
       ' ! ,
     ELSE
       !
     THEN
;

( n -- )
( adds n to the value specified to VAL )
: +TO IMMEDIATE
      WORD
      FIND
      >DFA
      8+
      STATE @ IF
        ' LIT ,
        ,
        ' +! ,
      ELSE
        +!
      THEN
;

: ID.
  8+
  DUP C@
  F_LENMASK AND

  BEGIN
    DUP 0>
  WHILE
    SWAP 1+
    DUP C@
    EMIT
    SWAP 1-
  REPEAT
  2DROP
;

: ?HIDDEN
  8+
  C@
  F_HIDDEN AND
;

: ?IMMEDIATE
  8+
  C@
  F_IMMED AND
;

: WORDS
  LATEST @
  BEGIN
    ?DUP
  WHILE
    DUP ?HIDDEN NOT IF
      DUP ID.
      SPACE
    THEN
    @
  REPEAT
  CR
;

: FORGET
  WORD FIND
  DUP @ LATEST !
  HERE !
;

( addr len -- )
: DUMP
  BASE @ -ROT
  HEX
  NEWLINE

  BEGIN
    ?DUP
  WHILE
    OVER 8 U.R
    SPACE

    2DUP
    1- 15 AND 1+
    BEGIN
      ?DUP
    WHILE
      SWAP
      DUP C@
      DUP 32 128 WITHIN IF
        EMIT
      ELSE
        DROP '.' EMIT
      THEN
      1+ SWAP 1-
    REPEAT
    DROP
    NEWLINE

    DUP 1- 15 AND 1+
    TUCK
    -
    >R + R>
  REPEAT
  DROP
  BASE !
;

: CASE IMMEDIATE
       0
;

: OF IMMEDIATE
     ' OVER ,
     ' = ,
     [COMPILE] IF
     ' DROP ,
;

: ENDOF IMMEDIATE
        [COMPILE] ELSE
;

: ENDCASE IMMEDIATE
          ' DROP ,
          BEGIN
            ?DUP
          WHILE
            [COMPILE] THEN
          REPEAT
;

: CFA>
  LATEST @
  BEGIN
    ?DUP
  WHILE
    2DUP SWAP
    16 - = IF
      NIP
      EXIT
    THEN
    @
  REPEAT
  DROP
  0
;

: SEE
  WORD FIND

  HERE @
  LATEST @
  BEGIN
    2 PICK
    OVER
    <>
  WHILE
    NIP
    DUP @
  REPEAT

  DROP
  SWAP

  ':' EMIT SPACE DUP ID. SPACE
  DUP ?IMMEDIATE IF ." IMMEDIATE " THEN
  >DFA

  BEGIN
    2DUP >
  WHILE
    DUP @

    CASE
      ' LIT OF
        8 + DUP @
        .
      ENDOF
      ' LITSTRING OF
        [ CHAR S ] LITERAL EMIT '"' EMIT SPACE 
        8 + DUP @
        SWAP 8 + SWAP
        2DUP TELL
        '"' EMIT SPACE
        + ALIGNED
        8 -
      ENDOF
      ' 0BRANCH OF
        ." 0BRANCH ( "
        8 + DUP @
        .
        ." ) "
      ENDOF
      ' BRANCH OF
        ." BRANCH ( "
        8 + DUP @
        .
        ." ) "
      ENDOF
      ' EXIT OF
        2DUP
        8 +
        <> IF
          ." EXIT "
        THEN
      ENDOF
      DUP
      CFA>
      ID. SPACE
    ENDCASE
    8 +
  REPEAT

  ';' EMIT CR

  2DROP
;

: :NONAME
    0 0 CREATE
    HERE @
    DOCOL ,
    ]
;

: ['] IMMEDIATE
    ' LIT ,
;

: EXCEPTION-MARKER
  RDROP
  0
;

: CATCH
  DSP@ 8+ >R
  ' EXCEPTION-MARKER 8+
  >R
  EXECUTE
;

: THROW
  ?DUP IF
    RSP@
    BEGIN
      DUP R0 8- <
    WHILE
      DUP @
      ' EXCEPTION-MARKER 8+ = IF
        8+
        RSP!

        DUP DUP DUP

        R>
        8-
        SWAP OVER
        !
        DSP! EXIT
      THEN
      8+
    REPEAT

    DROP

    CASE
      0 1- OF ( ABORT )
        ." ABORTED " CR
      ENDOF
      ( DEFAULT FALLTHROUGH )
      ." UNCAUGHT THROW "
      DUP . CR
    ENDCASE
    QUIT
  THEN
;

: ABORT
  0 1- THROW
;

: PRINT-STACK-TRACE
  RSP@
  BEGIN
    DUP R0 8- <
  WHILE
    DUP @
    CASE
      EXCEPTION-MARKER 8- OF
        ." CATCH ( DSP="
        8+ DUP @ U.
        ." ) "
      ENDOF
      ( DEFAULT CASE )
      DUP
      CFA>
      ?DUP IF
        2DUP
        ID.
        [ CHAR + ] LITERAL EMIT
        SWAP >DFA 8+ - .
      THEN
    ENDCASE
    8+
  REPEAT
  DROP
  CR
;

: UNUSED
  D0 1024 1024 * +
  HERE @
  -
  8 /
;

: WELCOME
  S" TEST-MODE" FIND NOT IF
    ." NICKFORTH VERSION " VERSION . NEWLINE
    UNUSED . ." CELLS REMAINING" NEWLINE
    ." OK "
    NEWLINE
  THEN
;

CLRPAGE
WELCOME
HIDE WELCOME
CLRLINE
