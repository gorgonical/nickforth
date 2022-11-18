  .set NICK_VERSION,1

serial_port_addr:
  .word 0x0

  /*
  If we assume we have the list of addresses to jump to at some point
	in memory, say 0x1000, we need a register that points there and we
	can jump to and increment.

  I think in the absence of a good reason we can use gp.

  That means at any given point gp contains the memory address which
	contains the address of the next instructions to jump to:

0x1000:
+00  08 00 00 00
+08  1C 00 00 00
+16  2C 00 00 00

  What we want is gp to have these contents
  0x1000, which we load 0x08 from.
  0x1008, which we load 0x1C from.
  0x1010, which we load 0x2C from.

  But because we have the codeword at the beginning of each
	definition, we need to actually load twice.
  */

  /* After:
  t0 contains codeword
  t1 contains address of what codeword pointed to (DOCOL or asm)

  %esi points to the next codeword, which is equivalent to gp.

  */
  .macro NEXT
  lw   t0, (gp)    /* Load address of the next codeword from gp */
  lw   t1, (t0)    /* Load whatever the codeword points to */
  addi gp, gp, 8   /* Increment gp */
  jr   t1          /* Jump to t0, what the codeword pointed to. */
  .endm

  /* We use fp for the return stack. */

  /* Stack pointer always points to the last thing pushed. */
  .macro PUSHRSP reg
  addi fp, fp, -8
  sw \reg, (fp)
  .endm

  .macro POPRSP reg
  lw \reg, (fp)
  addi fp, fp, 8
  .endm

  .text
  .align 8

  /* t0 points to the codeword after calling NEXT, which means we can
	advance a word to enter the definition. */
name_0:
DOCOL:
  PUSHRSP gp
  addi gp, t0, 8

  /* Assembler entry point. */
  .text
  .globl _start
_start:
  la fp, return_stack_top
  call set_up_data_segment

  la gp, cold_start
  NEXT

  .section .rodata
cold_start:
  .int QUIT

  /* Flags */
  .set F_IMMED,   0x80 /* Bits 3 and 2 ?? */
  .set F_HIDDEN,  0x20 /* Bit  1 */
  .set F_LENMASK, 0x1f

/* Assembler macro that eases defining words in FORTH */
  .macro defword name, namelen, flags=0, label, lastlink=0
  .section .rodata
  .align 8
  .globl name_\label
name_\label:
  .int name_\lastlink
  .byte \flags+\namelen
  .ascii "\name"
  .align 8
  .globl \label
\label:
  .int DOCOL
  /* Word pointers follow below */
  .endm

/* Assembler macro that eases defining words in assembly */
  .macro defcode name, namelen, flags=0, label, lastlink=0
  .section .rodata
  .align 8
  .globl name_\label
name_\label:
  .int name_\lastlink
  .byte \flags+\namelen
  .ascii "\name"
  .align 8
  .globl \label
\label:
  .int code_\label
  .text
  /* Align 8??? */
  .globl code_\label
code_\label:
  .endm

/*
  We use the regular sp for the parameter stack.
*/

  defcode "DROP",4,,DROP,0
  addi sp, sp, 8
  NEXT

  defcode "SWAP",4,,SWAP,DROP
  lw a0, (sp)
  lw a1, 8(sp)
  sw a0, 8(sp)
  sw a1, (sp)
  NEXT

  defcode "DUP",3,,DUP,SWAP
  lw a0, (sp)
  addi sp, sp, -8
  sw a0, (sp)
  NEXT

  /* DUP, but the second element of the stack. */
  defcode "OVER",4,,OVER,DUP
  lw a0, 8(sp)
  addi sp, sp, -8
  sw a0, (sp)
  NEXT

  /* Given stack elements 1<-2<-3, re-arrange them to be 3<-1<-2 */
  defcode "ROT",3,,ROT,OVER
  lw a0, (sp)
  lw a1, 8(sp)
  lw a2, 16(sp)
  sw a1, 16(sp)
  sw a0, 8(sp)
  sw a2, (sp)
  NEXT

  /* Given stack elements 1<-2<-3, re-arrange them to be 2<-3<-1 */
  defcode "-ROT",4,,NROT,ROT
  lw a0, (sp)
  lw a1, 8(sp)
  lw a2, 16(sp)
  sw a0, 16(sp)
  sw a2, 8(sp)
  sw a1, (sp)
  NEXT

  defcode "2DROP",5,,TWODROP,NROT
  addi sp, sp, 16
  NEXT

  defcode "2DUP",4,,TWODUP,TWODROP
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, -16
  sw a1, 8(sp)
  sw a0, (sp)
  NEXT

  /* Swap top two pairs of stack: 1234 -> 3412 */
  defcode "2SWAP",5,,TWOSWAP,TWODUP
  lw a0, (sp)
  lw a1, 8(sp)
  lw a2, 16(sp)
  lw a3, 24(sp)
  sw a2, (sp)
  sw a3, 8(sp)
  sw a0, 16(sp)
  sw a1, 24(sp)
  NEXT

  /* Duplicate top of stack if its non-zero */
  defcode "?DUP",4,,QDUP,TWOSWAP
  lw a0, (sp)
  beqz a0, 1f
  addi sp, sp, -8
  sw a0, (sp)
1:NEXT

  defcode "1+",2,,INCR,QDUP
  lw a0, (sp)
  addi a0, a0, 1
  sw a0, (sp)
  NEXT

  defcode "1-",2,,DECR,INCR
  lw a0, (sp)
  addi a0, a0, -1
  sw a0, (sp)
  NEXT

  defcode "4+",2,,INCR4,DECR
  lw a0, (sp)
  addi a0, a0, 4
  sw a0, (sp)
  NEXT

  defcode "4-",2,,DECR4,INCR4
  lw a0, (sp)
  addi a0, a0, -4
  sw a0, (sp)
  NEXT

  defcode "+",1,,ADD,DECR4
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  add a1, a0, a1
  sw a1, (sp)
  NEXT

  defcode "-",1,,SUB,ADD
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  sw a1, (sp)
  NEXT

  defcode "*",1,,MUL,SUB
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  mul a1, a0, a1
  sw a1, (sp)
  NEXT

/*
  Unlike in x86, RISC-V has no idiv instruction that produces both the
	quotient and remainder. So unlike in the original Jonesforth there's
	no reason to implement mod and div as Forth words on top of the
	single primitive.
*/

  defcode "DIV",1,,DIV,MUL
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  div a1, a1, a0
  sw a1, (sp)
  NEXT

  defcode "MOD",4,,MOD,DIV
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  rem a1, a1, a0
  sw a1, (sp)
  NEXT

  defcode "=",1,,EQU,MOD
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  seqz a1, a1
  sw a1, (sp)
  NEXT

  defcode "<>"2,,NEQU,EQU
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  snez a1, a1
  sw a1, (sp)
  NEXT

  defcode "<",1,,LT,NEQU
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  sltz a1, a1
  sw a1, (sp)
  NEXT

  defcode ">",1,,GT,LT
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  sgtz a1, a1
  sw a1, (sp)
  NEXT

  /* RISC-V does not have compound comparisons so we have to achieve
	it with either less than zero or equal zero comparisons */
  defcode "<=",2,,LE,GT
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  seqz a0, a1
  sltz a1, a1
  or a1, a1, a0
  sw a1, (sp)
  NEXT

  defcode ">=",2,,GE,LE
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  seqz a0, a1
  sgtz a1, a1
  or a1, a1, a0
  sw a1, (sp)
  NEXT

  defcode "0=",2,,ZEQU,GE
  lw a0, (sp)
  seqz a0, a0
  sw a0, (sp)
  NEXT

  defcode "0<>"3,,ZNEQU,ZEQU
  lw a0, (sp)
  snez a0, a0
  sw a0, (sp)
  NEXT

  defcode "0<",2,,ZLT,ZNEQU
  lw a0, (sp)
  sltz a0, a0
  sw a0, (sp)
  NEXT

  defcode "0>",2,,ZGT,ZLT
  lw a0, (sp)
  sgtz a0, a0
  sw a0, (sp)
  NEXT

  defcode "0<=",3,,ZLE,ZGT
  lw a0, (sp)
  sltz a1, a0
  seqz a0, a0
  or a0, a0, a1
  sw a0, (sp)
  NEXT

  defcode "0>=",3,,ZGE,ZLE
  lw a0, (sp)
  sgtz a1, a0
  seqz a0, a0
  or a0, a0, a1
  sw a0, (sp)
  NEXT

  defcode "AND",3,,AND,ZGE
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  and a0, a0, a1
  sw a0, (sp)
  NEXT

  defcode "OR",2,,OR,AND
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  or a0, a0, a1
  sw a0, (sp)
  NEXT

  defcode "XOR",3,,XOR,OR
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 8
  xor a0, a0, a1
  sw a0, (sp)
  NEXT

  defcode "INVERT",6,,INVERT,XOR
  lw a0, (sp)
  not a0, a0
  sw a0, (sp)
  NEXT

  defcode "EXIT",4,,EXIT,INVERT
  POPRSP gp
  NEXT

  /*
  To solve the literal problem, the LIT codeword needs to do some gp
	manipulation. It needs to push what gp points to next onto the stack
	and then advance past that.
  */
  defcode "LIT",3,,LIT,EXIT
  lw a0, (gp)
  addi gp, gp, 8
  sw a0, (sp)
  addi sp, sp, 8
  NEXT

  /* Here are all the memory manipulation words. */

  defcode "!",1,,STORE,LIT
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 16
  sw a1, (a0)
  NEXT

  defcode "@",1,,FETCH,STORE
  lw a0, (sp) // address
  lw a1, (a0) // fetched
  sw a1, (sp) //store on stack
  NEXT

  defcode "+!",2,,ADDSTORE,FETCH
  lw a0, (sp)  // address
  lw a1, 8(sp) // amt to add
  addi sp, sp, 16
  lw a2, (a0)
  add a2, a2, a1
  sw a2, (a0)
  NEXT

  defcode "-!",2,,SUBSTORE,ADDSTORE
  lw a0, (sp)
  lw a1, 8(sp)
  addi sp, sp, 16
  lw a2, (a0)
  sub a2, a2, a1
  sw a2, (a0)
  NEXT

  /* And some that operate on bytes only */

  defcode "C!",2,,STOREBYTE,SUBSTORE
  lw a0, (sp)  // address
  lw a1, 8(sp) // byte
  addi sp, sp, 16
  sb a1, (a0)
  NEXT

  defcode "C@",2,,FETCHBYTE,STOREBYTE
  lw a0, (sp)
  lb a1, (a0)
  sw a1, (sp)
  NEXT

  /* Are these supposed to preserve the addresses? */
  defcode "C@C!",4,,CCOPY,FETCHBYTE
  lw a0, (sp) // source addr
  lw a1, 8(sp) // dest addr
  lb a2, (a0) // byte to copy
  sb a2, (a1)
  addi a0, a0, 1 // Increment dest addr
  addi a1, a1, 1 // Increment source addr
  sw a0, (sp)
  sw a1, 8(sp)
  NEXT

  defcode "CMOVE",5,,CMOVE,CCOPY
  lw a0, (sp)   // length
  lw a1, 8(sp)  // dest
  lw a2, 16(sp) // src
1:
  lb a3, (a2)
  sb a3, (a1)
  li a4, 1
  sub a0, a0, a4
  bgtz a0, 1b
  NEXT

  /* We need to define a few built-in variables. We'll need a macro for that. */

  .macro defvar name, namelen, flags=0, label, prev, initial=0
  defcode \name,\namelen,\flags,\label,\prev
  la t0, var_\name
  sw t0, (sp)
  addi sp, sp, 8
  NEXT
  .data
  .align 4
var_\name:
  .int \initial
  .endm

  /*
  The built-in variables are:

  STATE   Is the interpreter executing code (0) or compiling a word (non-zero)?
  LATEST  Points to the most-recently defined word in the dictionary.
  HERE    Points to the next free byte of memory. Compiled words go here
  S0      Stores the address of the *top* of the parameter stack.
  BASE    The current base for printing and reading numbers.
  */

  defvar "STATE",5,,STATE,CMOVE
  defvar "HERE",4,,HERE,STATE
/*  defvar "LATEST",6,,LATEST,HERE,name_SYSCALL0 */
  defvar "S0",2,,S0,HERE
  defvar "BASE",4,,BASE,S0,10

  /*
  Built-in constants are also useful, and are a degenerate case of variables.
  */

  .macro defconst name, namelen, flags=0, label, value, prev
  defcode \name,\namelen,\flags,\label,\prev
  li t0, \value
  sw t0, (sp)
  addi sp, sp, 8
  NEXT
  .endm

  .macro defconst_addr name, namelen, flags=0, label, value, prev
  defcode \name,\namelen,\flags,\label,\prev
  la t0, \value
  sw t0, (sp)
  addi sp, sp, 8
  NEXT
  .endm


  defconst "VERSION",7,,VERSION,NICK_VERSION,BASE
  defconst_addr "R0",2,,RZ,return_stack_top,VERSION
  defconst_addr "DOCOL",5,,__DOCOL,DOCOL,RZ
  defconst "F_IMMED",7,,__F_IMMED,F_IMMED,__DOCOL
  defconst "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN,__F_IMMED
  defconst "F_LENMASK",9,,__F_LENMASK,F_LENMASK,__F_HIDDEN

  /* Unclear if these will be used since we are targeting bare-metal. */
/*
  defconst "SYS_EXIT",8,,SYS_EXIT,__NR_exit,__F_LENMASK
  defconst "SYS_OPEN",8,,SYS_OPEN,__NR_open,SYS_EXIT
  defconst "SYS_CLOSE",9,,SYS_CLOSE,__NR_close,SYS_OPEN
  defconst "SYS_READ",8,,SYS_READ,__NR_read,SYS_CLOSE
  defconst "SYS_WRITE",9,,SYS_WRITE,__NR_write,SYS_READ
  defconst "SYS_CREAT",9,,SYS_CREAT,__NR_creat,SYS_WRITE
  defconst "SYS_BRK",7,,SYS_BRK,__NR_brk,SYS_CREAT
*/
	defconst "O_RDONLY",8,,__O_RDONLY,0,__F_HIDDEN
	defconst "O_WRONLY",8,,__O_WRONLY,1,__O_RDONLY
	defconst "O_RDWR",6,,__O_RDWR,2,__O_WRONLY
	defconst "O_CREAT",7,,__O_CREAT,0100,__O_RDWR
	defconst "O_EXCL",6,,__O_EXCL,0200,__O_CREAT
	defconst "O_TRUNC",7,,__O_TRUNC,01000,__O_EXCL
	defconst "O_APPEND",8,,__O_APPEND,02000,__O_TRUNC
	defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000,__O_APPEND

  /*
  Accessing the return stack

  These words allow access to the return stack. Remember we use fp for that.
  */

  defcode ">R",2,,TOR,__O_NONBLOCK
  lw a0, (sp)
  addi sp, sp, 8
  PUSHRSP a0
  NEXT

  defcode "R<",2,,FROMR,TOR
  POPRSP a0
  sw a0, (sp)
  addi sp, sp, -8
  NEXT

  defcode "RSP@",4,,RSPFETCH,FROMR
  sw fp, (sp)
  addi sp, sp, -8
  NEXT

  defcode "RSP!",4,,RSPSTORE,RSPFETCH
  lw fp, (sp)
  addi sp, sp, 8
  NEXT

  /* This one just modifies the return stack directly, instead of sp->fp stuff. */
  defcode "RDROP",5,,RDROP,RSPSTORE
  addi fp, fp, 8
  NEXT

  /*
  Parameter stack manipulation. I don't know if I need this... ?
  */

  /* This just pushes the next availlable spot on the parameter stack??? */
  defcode "DSP@",4,,DSPFETCH,RDROP 
  mv sp, a0
  sw a0, (sp)
  NEXT

  /* Replaces the current parameter stack with whatever's on the top
	of the parameter stack. */
  defcode "DSP!",4,,DSPSTORE,DSPFETCH
  lw sp, (sp)
  NEXT

  /*

	The implementation of KEY uses an input buffer of a certain size (defined at the end of this
	file).  It calls the Linux read(2) system call to fill this buffer and tracks its position
	in the buffer using a couple of variables, and if it runs out of input buffer then it refills
	it automatically.  The other thing that KEY does is if it detects that stdin has closed, it
	exits the program, which is why when you hit ^D the FORTH system cleanly exits.

	     buffer			      bufftop
	|				 |
	V				 V
	+-------------------------------+--------------------------------------+
	| INPUT READ FROM STDIN ....... | unused part of the buffer            |
	+-------------------------------+--------------------------------------+
	                  ^
			  |
		       currkey (next character to read)

	<---------------------- BUFFER_SIZE (4096 bytes) ---------------------->

  From Original Jonesforth:

	defcode "KEY",3,,KEY
	call _KEY
	push %eax		// push return value on stack
	NEXT
_KEY:
	mov (currkey),%ebx
	cmp (bufftop),%ebx
	jge 1f			// exhausted the input buffer?
	xor %eax,%eax
	mov (%ebx),%al		// get next key from input buffer
	inc %ebx
	mov %ebx,(currkey)	// increment currkey
	ret

1:	// Out of input; use read(2) to fetch more input from stdin.
	xor %ebx,%ebx		// 1st param: stdin
	mov $buffer,%ecx	// 2nd param: buffer
	mov %ecx,currkey
	mov $BUFFER_SIZE,%edx	// 3rd param: max length
	mov $__NR_read,%eax	// syscall: read
	int $0x80
	test %eax,%eax		// If %eax <= 0, then exit.
	jbe 2f
	addl %eax,%ecx		// buffer+%eax = bufftop
	mov %ecx,bufftop
	jmp _KEY

	2:	// Error or end of input: exit the program.
	xor %ebx,%ebx
	mov $__NR_exit,%eax	// syscall: exit
	int $0x80

	.data
	.align 4
currkey:
	.int buffer		// Current place in input buffer (next character to read).
bufftop:
	.int buffer		// Last valid data in input buffer + 1.


  This will have to be reworked, as it relies on Linux for a lot of
	things: input handling, bufer management, signalling, etc.

	*/

  /* We expect the return value to be on... a0? */
  defcode "KEY",3,,KEY,DSPSTORE
  call _KEY
  sw a0, (sp)
  addi sp, sp, -8
  NEXT

  /* What does key need to do? It needs to take the next byte from
	stdin and push it onto the parameter stack. For us that means
	reading from the input buffer of a serial device.

  Probably we don't want (at first?) interrupts and will settle for polling.
*/
_KEY:
  la a1, serial_port_addr // Load address of variable containing address of serial port.
  lw a1, (a1)                 // Load serial port's address.
  /* Need to check read to receive? */
  lb a0, (a1)                 // Fetch byte from recv buffer. Offset is 0.
  /* Need to put the fetched byte into a0 */
  ret

  /* Only 1 param, the byte to write. Unlike the original Jonesforth. */
  defcode "EMIT",4,,EMIT,KEY
  lw a0, (sp)
  addi sp, sp, 8
  call _EMIT
  NEXT

_EMIT:
  la a1, serial_port_addr
  lw a1, (a1)
  sb a0, (a1) /* Store the byte to the recv/transmit buffer */
  ret

  defcode "WORD",4,,WORD,EMIT
  call _WORD
  sw a0,  (sp)  /* Base address of buffer */
  sw a1, 8(sp) /* Length of read word */
  addi sp, sp, 16
  NEXT

_WORD:

  /* Search for first non-blank character. Also skip \ comments. */
1:
  call _KEY /* Next key is now in a0 */
  li a2, '\\'
  sub a2, a0, a2
  /* If key == \ */
  beqz a2, 3f
  /* else if key == ' ' */
  li a2, ' '
  sub a2, a0, a2
  blez a2, 1b

  /* Search for the end of the word, storing characters as we go. */
  la a2, word_buffer
2:
  sw a0, (a2)
  addi a2, a2, 8 /* Increment the buffer position. */
  call _KEY
  li a1, ' '
  sub a1, a0, a1 /* Is it blank? */
  beqz a1, 1b    /* If so, keep looking. */

  /* Return the word (the static buffer) and its length */
  la a0, word_buffer
  sub a1, a2, a0 /* a2 now contains the length in bytes. */
  ret

  /* Code to skip \ comments to end of the current line. */
3:
  call _KEY /* Get next character */
  li a1, '\n'
  sub a1, a0, a1
  beqz a1, 3b /* If it's not the end of the line, go back. */
  j 1b

  .data
word_buffer:
  .space 32


  /* Returns two things on the stack: the number of unconverted
	characters (thus 0 indicates success), and second is the parsed
	number itself, or a partial value. */
  defcode "NUMBER",6,,NUMBER,WORD
  lw a0, (sp) /* Length of string */
  lw a1, 8(sp) /* Base address */
  call _NUMBER
  sw a0, 8(sp) /* Number of unparsed characters */
  sw a2, (sp) /* Parsed number ends up on top of stack */
  NEXT

  /* Basic strategy:
  1. Check for negative sign
  2. Convert each ascii byte to its numeric value
  3. Shift to the current place value in the digit.
  4. Decrement the length of string as we go along
  */

_NUMBER:
  mv a2, zero

  beqz a0, number_done /* Parsing a zero-length string is an error, and we return 0. */

  /* We parse numbers according to the variable BASE */
  la t6, var_BASE

  /* Check if the first character is a -. If it isn't, jump ahead. */
  lb t0, (a1)
  addi a1, a1, 1
  li t1, '-'
  sub t3, t0, t1 /* If t3 == 0, then the first character was negative. */
  addi a0, a0, -1
  bnez a0, digit_read_loop

  /* String is only '-' */
  li a2, 0
  li a3, 1
  ret

digit_read_loop:
  mul a2, a2, t6 /* a2 *= BASE */
  lb t0, (a1)
  addi a1, a1, 1

convert_digit:
  /* If the digit is less than '0' it is out of range. */
  addi t1, t0, -'0'
  bltz t1, need_negate
  /* Now <=0, check <= 9 */
  addi t1, t0, -'9'
  blez t1, got_digit
  /* Now this part checks for hex. We know it's > 9 and > 0. */
  addi t1, t0, -'A'
  bltz t1, need_negate
  /* Otherwise 10 to convert from hex. */
  addi t1, t1, 10
  /* Fall through to got_digit */

got_digit:
  /* If digit >= base, error */
  sub t1, t0, t6
  beqz t1, need_negate

  /* Otherwise it's fine, so add to a2 and go again */
  add a2, a2, t0
  addi a0, a0, -1
  bnez a0, digit_read_loop

  /* We get here when we're done either because no more characters or an error. If t3 */
need_negate:
  bnez t3, number_done
  neg a2, a2

number_done:
  ret

  /* Check if the first character is '-'   */
set_up_data_segment:

return_stack_top:

QUIT:

