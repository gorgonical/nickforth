/* -*- tab-width: 2 -*-  */
  .set NICK_VERSION,1

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

/*
  Registers used:
	  - gp: The address of the next codeword. While running a word, gp
	points to the definition of the word. Thus when a word is finished
	executing it calls NEXT, which jumps to execution of the next word
	in the definition. Words that call other words must preserve gp,
	which DOCOL does by pushing the existing gp onto the return stack.
	EXIT restores the caller's gp and calls NEXT to resume execution
	there.

    - fp: This register always points to the return stack pointer.
	This could be elided by load/stores to store the latest, but we keep
	the top of the return stack here. We use this for exactly three things:
      1. Storing gp when we run a word from within a word.
      2. Storing ra when we `call` a procedure that we will `ret` from inside a word
      3. Four words that allow manipulating the return stack: >R, R>, RSP@, RSP!

    - sp: This is the ordinary parameter/data stack pointer that is
	used consistent with how everyone else uses sp, basically.

    - tp: Not used

    - ra: Used in the normal way, but only within SAVECALL and ret within a procedure.
*/
  .macro NEXT
  ld   t0, (gp)    /* Load address of the next codeword from gp */
  addi gp, gp, 8   /* Increment gp */
  ld t1,(t0) 
  jr t1 /* Jump to t1, what the codeword pointed to. */
  .endm

  /* We use fp for the return stack. */

  /* Stack pointer always points to the last thing pushed. */
  .macro PUSHRSP reg
  addi fp, fp, -8
  sd \reg, (fp)
  .endm

  .macro POPRSP reg
  ld \reg, (fp)
  addi fp, fp, 8
  .endm

  .macro SAVECALL label
  PUSHRSP ra
  call \label
  POPRSP ra
  .endm

  .text
  .align 3

  j _start

  /* t0 points to the codeword after calling NEXT, which means we can
	advance a word to enter the definition. */
name_0:
DOCOL:
  PUSHRSP gp
  addi gp, t0, 8
  NEXT

holding_pen:
  j holding_pen

vec:
  la t0, var_EXCPHAND
  ld t0, (t0)
  ld t1, (t0)
  jr t1
  NEXT

  /* Assembler entry point. */
  .text
  .globl _start
_start: 
  la t0, vec
  csrw stvec, t0
  
  la fp, return_stack_top
  la sp, param_stack_top

  la t0, var_HAVEDEFS
  ld t3, (t0)
  bnez t3, have_defs

no_defs:
  la t0, var_HERE
  la t1, data_segment_start
  sd t1, (t0)

  la t0, var_LATEST
  la t1, name_INTERPRET
  sd t1, (t0)

  j rejoin

have_defs:
  la t0, var_HERE
  li t1, 0x8021d608 /* fill in later */
  sd t1, (t0)

  la t0, var_LATEST
  li t1, 0x8021d518
  sd t1, (t0)

rejoin:
  la t0, var_S0
  sd sp, (t0)

  la t0, var_LINEMODE
  li t1, 0
  sd t1, (t0)

  la gp, cold_start

  la t0, serial_port_addr
  ld t0, (t0)
  li t1, 'R'
  sb t1, (t0)
  li t1, 'E'
  sb t1, (t0)
  li t1, 'S'
  sb t1, (t0)
  li t1, 'E'
  sb t1, (t0)
  li t1, 'T'
  sb t1, (t0)
  li t1, '\n'
  sb t1, (t0)

  NEXT

  .section .rodata
cold_start:
  .dword QUIT

  /* Flags */
  .set F_IMMED,   0x80 /* Bits 3 and 2 ?? */
  .set F_HIDDEN,  0x20 /* Bit  1 */
  .set F_LENMASK, 0x1f /* 0b 0001 1111*/

/* Assembler macro that eases defining words in FORTH */
  .macro defword name, namelen, flags=0, label, lastlink=0
  .section .rodata
  .align 3
  .globl name_\label
name_\label:
  .dword name_\lastlink
  .byte \flags+\namelen
  .ascii "\name"
  .align 3
  .globl \label
\label:
  .dword DOCOL
  /* Word pointers follow below */
  .endm

  .set name_ZERO,0
  
/* Assembler macro that eases defining words in assembly */
  .macro defcode name, namelen, flags=0, label, lastlink=0
  .section .rodata
  .align 3
  .globl name_\label
name_\label:
  .dword name_\lastlink
  .byte \flags+\namelen
  .ascii "\name"
  .align 3
  .globl \label
\label:
  .dword code_\label
  .text
  /* Align 8??? */
  .globl code_\label
code_\label:
  .endm

/*
  We use the regular sp for the parameter stack.
*/

  defcode "DROP",4,,DROP,ZERO
  addi sp, sp, 8
  NEXT

  defcode "SWAP",4,,SWAP,DROP
  ld a0, (sp)
  ld a1, 8(sp)
  sd a0, 8(sp)
  sd a1, (sp)
  NEXT

  defcode "DUP",3,,DUP,SWAP
  ld a0, (sp)
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

  /* DUP, but the second element of the stack. */
  defcode "OVER",4,,OVER,DUP
  ld a0, 8(sp)
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

  /* Given stack elements 1<-2<-3, re-arrange them to be 3<-1<-2 */
  defcode "ROT",3,,ROT,OVER
  ld a0, (sp)
  ld a1, 8(sp)
  ld a2, 16(sp)
  sd a1, 16(sp)
  sd a0, 8(sp)
  sd a2, (sp)
  NEXT

  /* Given stack elements 1<-2<-3, re-arrange them to be 2<-3<-1 */
  defcode "-ROT",4,,NROT,ROT
  ld a0, (sp)
  ld a1, 8(sp)
  ld a2, 16(sp)
  sd a0, 16(sp)
  sd a2, 8(sp)
  sd a1, (sp)
  NEXT

  defcode "2DROP",5,,TWODROP,NROT
  addi sp, sp, 16
  NEXT

  defcode "2DUP",4,,TWODUP,TWODROP
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, -16
  sd a1, 8(sp)
  sd a0, (sp)
  NEXT

  /* Swap top two pairs of stack: 1234 -> 3412 */
  defcode "2SWAP",5,,TWOSWAP,TWODUP
  ld a0, (sp)
  ld a1, 8(sp)
  ld a2, 16(sp)
  ld a3, 24(sp)
  sd a2, (sp)
  sd a3, 8(sp)
  sd a0, 16(sp)
  sd a1, 24(sp)
  NEXT

  /* Duplicate top of stack if its non-zero */
  defcode "?DUP",4,,QDUP,TWOSWAP
  ld a0, (sp)
  beqz a0, 1f
  addi sp, sp, -8
  sd a0, (sp)
1:NEXT

  defcode "1+",2,,INCR,QDUP
  ld a0, (sp)
  addi a0, a0, 1
  sd a0, (sp)
  NEXT

  defcode "1-",2,,DECR,INCR
  ld a0, (sp)
  addi a0, a0, -1
  sd a0, (sp)
  NEXT

  defcode "4+",2,,INCR4,DECR
  ld a0, (sp)
  addi a0, a0, 4
  sd a0, (sp)
  NEXT

  defcode "4-",2,,DECR4,INCR4
  ld a0, (sp)
  addi a0, a0, -4
  sd a0, (sp)
  NEXT

  defcode "+",1,,ADD,DECR4
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  add a1, a0, a1
  sd a1, (sp)
  NEXT

  defcode "-",1,,SUB,ADD
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  sd a1, (sp)
  NEXT

  defcode "*",1,,MUL,SUB
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  mul a1, a0, a1
  sd a1, (sp)
  NEXT

/*
  Unlike in x86, RISC-V has no idiv instruction that produces both the
	quotient and remainder. So unlike in the original Jonesforth there's
	no reason to implement mod and div as Forth words on top of the
	single primitive.
*/

  defcode "/MOD",4,,DIVMOD,MUL
  ld a0, (sp) /* Top of stack */
  ld a1, 8(sp)
  div a3, a1, a0
  rem a1, a1, a0
  sd a3, (sp)  /* top of stack: quotient */
  sd a1, 8(sp) /* after that: remainder  */
  NEXT

  defcode "=",1,,EQU,DIVMOD
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  seqz a1, a1
  sd a1, (sp)
  NEXT

  defcode "<>"2,,NEQU,EQU
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  snez a1, a1
  sd a1, (sp)
  NEXT

  defcode "<",1,,LT,NEQU
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  sltz a1, a1
  sd a1, (sp)
  NEXT

  defcode ">",1,,GT,LT
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  sgtz a1, a1
  sd a1, (sp)
  NEXT

  /* RISC-V does not have compound comparisons so we have to achieve
	it with either less than zero or equal zero comparisons */
  defcode "<=",2,,LE,GT
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  seqz a0, a1
  sltz a1, a1
  or a1, a1, a0
  sd a1, (sp)
  NEXT

  defcode ">=",2,,GE,LE
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  sub a1, a1, a0
  seqz a0, a1
  sgtz a1, a1
  or a1, a1, a0
  sd a1, (sp)
  NEXT

  defcode "0=",2,,ZEQU,GE
  ld a0, (sp)
  seqz a0, a0
  sd a0, (sp)
  NEXT

  defcode "0<>"3,,ZNEQU,ZEQU
  ld a0, (sp)
  snez a0, a0
  sd a0, (sp)
  NEXT

  defcode "0<",2,,ZLT,ZNEQU
  ld a0, (sp)
  sltz a0, a0
  sd a0, (sp)
  NEXT

  defcode "0>",2,,ZGT,ZLT
  ld a0, (sp)
  sgtz a0, a0
  sd a0, (sp)
  NEXT

  defcode "0<=",3,,ZLE,ZGT
  ld a0, (sp)
  sltz a1, a0
  seqz a0, a0
  or a0, a0, a1
  sd a0, (sp)
  NEXT

  defcode "0>=",3,,ZGE,ZLE
  ld a0, (sp)
  sgtz a1, a0
  seqz a0, a0
  or a0, a0, a1
  sd a0, (sp)
  NEXT

  defcode "AND",3,,AND,ZGE
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  and a0, a0, a1
  sd a0, (sp)
  NEXT

  defcode "OR",2,,OR,AND
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  or a0, a0, a1
  sd a0, (sp)
  NEXT

  defcode "XOR",3,,XOR,OR
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 8
  xor a0, a0, a1
  sd a0, (sp)
  NEXT

  defcode "INVERT",6,,INVERT,XOR
  ld a0, (sp)
  not a0, a0
  sd a0, (sp)
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
  ld a0, (gp)
  addi gp, gp, 8
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

  /* Here are all the memory manipulation words. */

  defcode "!",1,,STORE,LIT
  ld a0, (sp)  /* Address to store at */
  ld a1, 8(sp) /* Value to store */
  addi sp, sp, 16
  sd a1, (a0)
  NEXT

  defcode "@",1,,FETCH,STORE
  ld a0, (sp) // address
  ld a1, (a0) // fetched
  sd a1, (sp) //store on stack
  NEXT

  defcode "W!",2,,STOREWORD,FETCH
  ld a0, (sp)
  mv a1, zero
  ld a1, 8(sp)
  addi sp, sp, 16
  sw a1, (a0)
  NEXT

  defcode "W@",2,,LOADWORD,STOREWORD
  ld a0, (sp)
  mv a1, zero
  lw a1, (a0)
  sd a1, (sp)
  NEXT

  defcode "H!",2,,STOREHALF,LOADWORD
  ld a0, (sp)
  mv a1, zero
  ld a1, 8(sp)
  addi sp, sp, 16
  sh a1, (a0)
  NEXT

  defcode "H@",2,,LOADHALF,STOREHALF
  ld a0, (sp)
  mv a1, zero
  lh a1, (a0)
  sd a1, (sp)
  NEXT

  defcode "+!",2,,ADDSTORE,LOADHALF
  ld a0, (sp)  // address
  ld a1, 8(sp) // amt to add
  addi sp, sp, 16
  ld a2, (a0)
  add a2, a2, a1
  sd a2, (a0)
  NEXT

  defcode "-!",2,,SUBSTORE,ADDSTORE
  ld a0, (sp)
  ld a1, 8(sp)
  addi sp, sp, 16
  ld a2, (a0)
  sub a2, a2, a1
  sd a2, (a0)
  NEXT

  /* And some that operate on bytes only */

  defcode "C!",2,,STOREBYTE,SUBSTORE
  ld a0, (sp)  // address
  mv a1, zero
  lb a1, 8(sp) // byte
  addi sp, sp, 16
  sb a1, (a0)
  NEXT

  defcode "C@",2,,FETCHBYTE,STOREBYTE
  mv a1, zero
  ld a0, (sp)
  lb a1, (a0)
  sd a1, (sp)
  NEXT

  /* Are these supposed to preserve the addresses? */
  defcode "C@C!",4,,CCOPY,FETCHBYTE
  ld a0, (sp) // source addr
  ld a1, 8(sp) // dest addr
  mv a2, zero
  lb a2, (a0) // byte to copy
  sb a2, (a1)
  addi a0, a0, 1 // Increment dest addr
  addi a1, a1, 1 // Increment source addr
  sd a0, (sp)
  sd a1, 8(sp)
  NEXT

  defcode "CMOVE",5,,CMOVE,CCOPY
  ld a0, (sp)   // length
  ld a1, 8(sp)  // dest
  ld a2, 16(sp) // src
  addi sp, sp, 24
1:
  mv a3, zero
  ld a3, (a2)
  sd a3, (a1)
  addi a2, a2, 8
  addi a1, a1, 8
  addi a0, a0, -8
  bgtz a0, 1b
  NEXT

  /* We need to define a few built-in variables. We'll need a macro for that. */

  .macro defvar name, namelen, flags=0, label, prev, initial=0
  defcode \name,\namelen,\flags,\label,\prev
  la t0, var_\name
  addi sp, sp, -8
  sd t0, (sp)
  NEXT
  .data
  .align 3
var_\name:
  .dword \initial
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
  defvar "LINEMODE",8,,LINEMODE,STATE
  defvar "HERE",4,,HERE,LINEMODE
  defvar "LATEST",6,,LATEST,HERE,name_INTERPRET
  defvar "S0",2,,S0,LATEST
  defvar "EXCPHAND",8,,EXCPHAND,S0,
  defvar "BASE",4,,BASE,EXCPHAND,10
  defvar "HAVEDEFS",8,,HAVEDEFS,BASE,_HAVEDEFS

  /*
  Built-in constants are also useful, and are a degenerate case of variables.
  */

  .macro defconst name, namelen, flags=0, label, value, prev
  defcode \name,\namelen,\flags,\label,\prev
  li t0, \value
  addi sp, sp, -8
  sd t0, (sp)
  NEXT
  .endm

  .macro defconst_addr name, namelen, flags=0, label, value, prev
  defcode \name,\namelen,\flags,\label,\prev
  la t0, \value
  addi sp, sp, -8
  sd t0, (sp)
  NEXT
  .endm


  defconst "VERSION",7,,VERSION,NICK_VERSION,HAVEDEFS
  defconst_addr "R0",2,,RZ,return_stack_top,VERSION
  defconst_addr "D0",2,,DZ,data_segment_start,RZ
  defconst_addr "DOCOL",5,,__DOCOL,DOCOL,DZ
  defconst_addr "LINEBUF",7,,LINEBUF,line_buffer,__DOCOL
  defconst "F_IMMED",7,,__F_IMMED,F_IMMED,LINEBUF
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
	defconst "O_RDONLY",8,,__O_RDONLY,0,__F_LENMASK
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
  ld a0, (sp)
  addi sp, sp, 8
  PUSHRSP a0
  NEXT

  defcode "R>",2,,FROMR,TOR
  POPRSP a0
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

  defcode "RSP@",4,,RSPFETCH,FROMR
  addi sp, sp, -8
  sd fp, (sp)
  NEXT

  defcode "RSP!",4,,RSPSTORE,RSPFETCH
  ld fp, (sp)
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
  mv a0, sp
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

  /* Replaces the current parameter stack with whatever's on the top
	of the parameter stack. */
  defcode "DSP!",4,,DSPSTORE,DSPFETCH
  ld sp, (sp)
  NEXT

  /*
  From Original Jonesforth:

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
  SAVECALL _KEY
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

  /* What does key need to do? It needs to take the next byte from
	stdin and push it onto the parameter stack. For us that means
	reading from the input buffer of a serial device.

  Probably we don't want (at first?) interrupts and will settle for polling.
*/
_KEY:
  /* Check if we are in linemode */
  la t0, var_LINEMODE
  ld t0, (t0)
  bnez t0, _KEY_LINEMODE

_KEY_CHARMODE:
  la a1, serial_port_addr // Load address of variable containing address of serial port.
  ld a1, (a1)                 // Load serial port's address.
  /* Until byte is ready, keep looping. If KEY is called and there are
	no bytes available, the intention is to loop forever. */
key_loop:
  mv a0, zero
  lbu a0, (a1)
  beqz a0, key_loop
  sb a0, (a1) /* Echo the byte back to the serial port. */
  ret

_KEY_LINEMODE:
  la a1, line_buffer
  la a2, curpos
  ld a3, (a2) /* Get offset */
  la t0, line_end
  ld t0, (t0)
  add a0, a1, a3
  sub t0, t0, a0 /* If curpos+line_buffer (a0) goes past line_end (t0), we are out of characters. */
  bltz t0, outofcharacters
  lbu a0, (a0)
  addi a3, a3, 1
  sd a3, (a2) /* Update curpos */
  ret

outofcharacters:
  li a0, 0 /* Load zero to indicate */
  ret

  /* Only 1 param, the byte to write. Unlike the original Jonesforth. */
  defcode "EMIT",4,,EMIT,KEY
  ld a0, (sp)
  addi sp, sp, 8
  SAVECALL _EMIT
  NEXT

_EMIT:
  la a1, serial_port_addr
  ld a1, (a1)
  sb a0, (a1) /* Store the byte to the recv/transmit buffer */
  ret

  /* WORD reads the next full word of input as a string. */
  defcode "WORD",4,,WORD,EMIT
  SAVECALL _WORD
  addi sp, sp, -16
  sd a1, 8(sp)  /* Base address of buffer */
  sd a0,  (sp)  /* Length of read word */
  NEXT

_WORD:

  /* Search for first non-blank character. Also skip \ comments. */
1:
  SAVECALL _KEY /* Next key is now in a0 */
  /* If key == \ */
  li a2, '\\'
  sub a2, a0, a2
  beqz a2, 3f
  /* else if key == ' ' */
  li a2, ' '
  sub a2, a0, a2
  blez a2, 1b
  /* else if key == '\n' */
  li a2, '\n'
  sub a2, a0, a2
  blez a1, 1b


  /* Search for the end of the word, storing characters as we go. */
  la a2, word_buffer
2:
  sb a0, (a2)
  addi a2, a2, 1 /* Increment the buffer position. */
  mv s6, a2
  SAVECALL _KEY
  mv a2, s6
  li a1, ' '
  sub a1, a0, a1 /* Is it blank? If not, keep reading */
  beqz a1, end_of_word
  li a1, '\r'
  sub a1, a0, a1
  beqz a1, end_of_word
  j 2b
end_of_word:

  /* Return the word (the static buffer) and its length */
  la a1, word_buffer
  sub a0, a2, a1 /* a1 now contains the length in bytes. */
  ret

  /* Code to skip \ comments to end of the current line. */
3:
  SAVECALL _KEY /* Get next character */
  li a1, '\n'
  sub a1, a0, a1
  beqz a1, 3b /* If it's not the end of the line, go back. */
  j 1b

  .data
word_buffer:
  .space 64

  defcode "LINE",4,,LINE,WORD
  SAVECALL _LINE
  addi sp, sp, -8
  sd a0, (sp)
  NEXT

_LINE:
  /* Fetch characters, storing them in the linebuffer until we find
	the newline character or a backspace. */
  la a3, line_buffer
  la a6, curpos

  /* If we still have stuff in the line we should use it instead of fetching more */
  la a4, line_end
  ld a4, (a4)
  ld a6, (a6)    /* Get curpos */
  add a6, a6, a3 /* Get where we are currently in the line_buffer */
  sub a5, a4, a6 /* Number of characters */
  blez a5, reset_line
  ret            /* If there's still characters, immediately return */
reset_line:
  /* Reset curpos, too */
  la a6, curpos
  li a1, 0
  sd a1, (a6)
1:
  SAVECALL _KEY_CHARMODE /* key in a0 */
line_get:
  li a1, 0x8
  sub a1, a0, a1
  beqz a1, line_bs

  sb a0, (a3)
  addi a3, a3, 1

  li a1, '\r'
  sub a1, a0, a1
  beqz a1, line_done

  /* Should think about what happens if the line is really long. */

  j 1b

line_done:
  /* Get length of line in characters */
  la a1, line_buffer
  sub a0, a3, a1 /* a0 now has number of characters in it */
  addi a0, a0, -1
  beqz a0, _LINE
  add a0, a1, a0
  la a4, line_end
  sd a0, (a4)
  li a0, '\r'
  SAVECALL _EMIT
  li a0, '\n'
  SAVECALL _EMIT
  ret

line_bs:
  addi a3, a3, -1
  li a0, 0
  sb a0, (a3)
  li a0, ' '
  SAVECALL _EMIT
  li a0, 0x8
  SAVECALL _EMIT
  j 1b

line_end:
  .dword 0
curpos:
  .dword 0
line_buffer:
  .space 256

  /* Returns two things on the stack: the number of unconverted
	characters (thus 0 indicates success), and second is the parsed
	number itself, or a partial value. */
  defcode "NUMBER",6,,NUMBER,LINE
  ld a0, (sp) /* Length of string */
  ld a1, 8(sp) /* Base address */
  SAVECALL _NUMBER
  sd a0, 8(sp) /* Number of unparsed characters */
  sd a2, (sp) /* Parsed number ends up on top of stack */
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
  lw t6, (t6)

  /* Check if the first character is a -. If it isn't, jump ahead. */
  lb t0, (a1)
  li t1, '-'
  sub t3, t0, t1 /* If t3 == 0, then the first character was negative. */
  bnez t3, digit_read_loop /* If t3 != '-', jump to the parse loop. */
  addi a1, a1, 1 /* If the first byte is '-' then we need to advance the byte... */
  addi a0, a0, -1 /* And decrement how many bytes to read. */
  bnez a0, digit_read_loop /* If any characters left, jump to read loop. */

  /* String is only '-' */
  li a2, 0
  li a3, 1
  ret

digit_read_loop:
  mul a2, a2, t6 /* a2 *= BASE */
  mv t0, zero
  lb t0, (a1)
  addi a1, a1, 1

convert_digit:
  /* If the digit is less than '0' it is out of range. */
  addi t1, t0, -'0'
  bltz t1, need_negate
  /* Now the digit is <=0, now check >= 9 */
  addi t2, t0, -'9'
  blez t2, got_digit /* t1 now contains the digit that's 0 <= x <= 9 */
  /* Now this part checks for hex. We know it's > 9 and > 0. */
  addi t1, t0, -'A' /* Convert t0 to digit from hex */
  bltz t1, need_negate /* Error? */
  /* Otherwise 10 to convert from hex. */
  addi t1, t1, 10
  /* Fall through to got_digit */

got_digit:
  /* If digit >= base, error */
  sub t2, t1, t6
  bgez t2, need_negate

  /* Otherwise it's fine, so add to a2 and go again */
  add a2, a2, t1
  addi a0, a0, -1
  bnez a0, digit_read_loop

  /* We get here when we're done either because no more characters or an error.  */
need_negate:
  /* If t3 == 0, the number is negative. Skip this if t3 DOES NOT equal zero. */
  bnez t3, number_done
  neg a2, a2

number_done:
  ret

  /* Now we implement FIND, the way to find a dictionary entry for a
	word indicated by a string. */

  defcode "FIND",4,,FIND,NUMBER
  ld a0, (sp)  /* Length of string */ // change to a0 
  ld a1, 8(sp) /* Address of string */// change to a1
  SAVECALL _FIND
  addi sp, sp, 8
  sd a2, (sp)
  NEXT

  /*
  +---------+---+---+---+---+---+---+---+---+------------+------------+------------+----------  --+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        | +          | EXIT       |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+

  Considering that a dictionary entry looks like this, we can work
	back through the chain by repeated loads:

name_C: [0x3000]
  .int name_B <-- 0x2000

name_B: [0x2000]
  .int name_A <-- 0x1000

name_A: [0x1000]
  .int 0

  The original Jonesforth uses a length to shortcut certain words. We
	do a cheaper numerical check against the length in the dictionary
	and if they don't match we move on.

  Makes me wonder if we could do something with a hash entry also.

  */
_FIND:
  la t0, var_LATEST
  ld t0, (t0)
find_loop:
  beqz t0, not_found
  mv t1, zero
  lb t1, 8(t0) /* Offset of 8 to index len+flags byte because we use 64-bit words for addresses */
  li t2, F_LENMASK|F_HIDDEN
  and t1, t1, t2
  sub t1, t1, a0
  bnez t1, next_word /* If the lengths don't match or hidden is set, go on to the next word. */

  /* Compare strings more carefully, starting from the first byte. */
  mv t1, t0
  addi t1, t1, 9
  mv t2, a1
  mv t6, a0 /* Copy length also */
next_character:
  mv t3, zero
  mv t4, zero
  lb t3, (t1) /* Load the bytes representing the characters */
  lb t4, (t2)
  sub t3, t3, t4
  bnez t3, next_word /* If they aren't the same, skip ahead */
  /* Else, increment them. */
  addi t1, t1, 1
  addi t2, t2, 1
  /* Loop until we reach the null byte */
  addi t6, t6, -1
  bnez t6, next_character

  /* If we get here we have checked every byte and they all matched. */
  mv a2, t0
  ret

  /* The LINK pointer (in our case, var_\LAST) is aldays at the
	beginning of the definition. That is, at 0x5000, the definition for
	DUP or whatever:

@0x5000  | 0x4000 | LEN+FLAGS | DUP\0 | ...
         ^
         |--- t0

  */
next_word:
  ld t0, (t0)
  j find_loop

not_found:
  li a2, 0
  ret

  /* Now we need >CFA, which takes the pointer to the dictionary
	defition and gets the actual executable part of the word, the
	codeword. */
  defcode ">CFA",4,,TCFA,FIND
  ld a0, (sp) /* The pointer to the beginning of the definition */
  SAVECALL _TCFA
  sd a0, (sp)
  NEXT

  /* If you lb into a register does it clear the upper bits? */
_TCFA:
  mv t0, zero
  lb t0, 8(a0) /* Load len+flags byte */
  li t1, F_LENMASK
  and t0, t0, t1 /* Mask to get the length. */
  add t1, a0, 9 /* Point to the name */
  add t1, t1, t0 /* Skip the length of the name */
  /* 8-byte align */
  addi t1, t1, 7
  and  a0, t1, ~7
  ret

  /* And also we will have a >DFA, which converts the DEA (dictionary entry address) to the data field address (DFA). */
  defword ">DFA",4,,TDFA,TCFA
  .dword TCFA
  .dword INCR4
  .dword INCR4
  .dword EXIT

  /* Now for compiling. We have to solve some problems:
      1. Where do new words go?
      2. How do we read words?
      3. How do we define COLON and SEMICOLON?

  So we define an INTERPRET function, wihch reads words, looks them
	up, turns them into codeword pointers, and decides what to do with
	them.

  When STATE is zero, the interpreter just runs the words as it looks
	them up: immediate mode.

  When STATE is in compoling mode when STATE is non-zero. The
	interpreter appends the codeword pointer to user memory (where the
	HERE variable points)

  So here's how we define COLON:
      1. WORD reads the name
      2. Construct the dictionary header in user memory.

    pointer to previous word (from LATEST)          			+-- Afterwards, HERE points here, where
	   ^                                      							|   the interpreter will start appending
	   |                                      							V   codewords.
	+--|------+---+---+---+---+---+---+---+---+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      |
	+---------+---+---+---+---+---+---+---+---+------------+
             len                         pad  codeword


      3. Make LATEST point to the newly defined word
      4. And make HERE point to just after the new codeword. New codewords go here.
      5. Set STATE to 1. This sets compile mode so the interpreter starts appending words.

  After : has run, this is where we are:
      :  DOUBLE DUP + ;
                ^
                | Next byte returned by KEY will be the 'D' character of DUP.

  Then DUP is rea d, looked up, and appended:

                                                									     +-- HERE updated to point here.
                                                									     |
                                                									     V
	+---------+---+---+---+---+---+---+---+---+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | DOCOL      | DUP        |
	+---------+---+---+---+---+---+---+---+---+------------+------------+
             len                         pad  codeword


  But how do we get out of compiling mode? We don't want ; to be read
	and appended. So we could either make the interpreter specially
	check if its SEMICOLON, or we can define a general way to do it. So,
	what the interpreter does is check the word's flags for F_IMMED. If
	that's set, the word is executed _right now_, regardless of STATE.

  So all SEMICOLON does is append EXIT, and set STATE to 0. 

  However, we actually write a part of COLON as its own word, CREATE, which we define first. 

  */

  defcode "CREATE",6,,CREATE,TDFA

  // Get name length and address
  ld a0, (sp)  // length
  ld a1, 8(sp) // address of name
  addi sp, sp, 16

  /*
    Always confusing when this is a double-pointer.
    Addr
    0x1000 == var_HERE: 0xfc00
    0x2000
    0x3000
    ...

    So,
      la t2, var_HERE
    means t2 now contains 0x1000. It contains the _address of_ var_HERE.
      lw t3, (t2)
    Loads the word from 0x1000 into t3. t3 now contains 0xfc00, the _value_ of var_HERE.
  */
  /* Need to get whatever the LATEST defined word is and start the new
	word header by writing it at where HERE points */
  la t0, var_LATEST
  ld t1, (t0)

  la t6, var_HERE
  ld t3, (t6)
  mv t5, t3 /* Copy the _value_ of HERE for later when we update LATEST */

  /* Now, store the _value_ of LATEST (a memory address) at the
	_value_ of HERE (also a memory address)
  */
  sd t1, (t3)
  /* Increment the _value_ of HERE by 8 (not written-back) */
  addi t3, t3, 8

  /* Store the length byte next, also at the _value_ of HERE. */
  sb a0, (t3)
  addi t3, t3, 1

  /* Store the actual word itself, byte by byte */
store_chars:
  beqz a0, no_more_chars
  mv t2, zero
  lb t2, (a1)    /* Get character from name */
  addi a1, a1, 1
  sb t2, (t3)    /* Put character at HERE   */
  addi t3, t3, 1

  addi a0, a0, -1
  j store_chars
no_more_chars:

  /* Align */
  addi t3, t3, 7
  and  t3, t3, ~7

  /* Update the _value_ of LATEST with what HERE was when we started,
	as it is now the LINK pointer to the most recently-defined word. */
  sd t5, (t0)
  /* Now, write back the _value_ of HERE. */
  sd t3, (t6)
  NEXT

  /* We need to define another word, COMMA, which appends a
	word-length integer to wherever HERE points. */
  defcode ",",1,,COMMA,CREATE
  ld a0, (sp)
  addi sp, sp, 8
  SAVECALL _COMMA
  NEXT

_COMMA:
  la t0, var_HERE
  ld t1, (t0)
  sd a0, (t1) // Store the word
  addi t1, t1, 8  // Increment and
  sd t1, (t0)     // update HERE
  ret

  /* We need to define some words that change the compiler state. */
  defcode "[",1,F_IMMED,LBRAC,COMMA
  la t0, var_STATE
  sd zero, (t0) // Set STATE to 0.
  NEXT

  defcode "]",1,,RBRAC,LBRAC
  la t0, var_STATE
  li t1, 1
  sd t1, (t0) // Set STATE to 1.
  NEXT


  /* Toggle the hidden flag of the word at addr. To change the latest:
      LATEST @ HIDDEN
  */
  defcode "HIDDEN",6,,HIDDEN,RBRAC
  ld a0, (sp)
  addi sp, sp, 8
  addi a0, a0, 8 // Skip to the name/flags byte
  mv t0, zero
  lb t0, (a0)
  xori t0, t0, F_HIDDEN
  sb t0, (a0)
  NEXT

  /* IMMEDIATE toggles the immediate flag of the most recently-defined word. */
  defcode "IMMEDIATE",9,F_IMMED,IMMEDIATE,HIDDEN
  la t0, var_LATEST // Load the _address_ of LATEST
  ld t1, (t0)       // Load the _value_ of LATEST
  addi t1, t1, 8    // Skip to the name/flags byte.
  mv t2, zero
  lb t2, (t1)
  xor t2, t2, F_IMMED
  sb t2, (t1)
  NEXT

  /* And now, we can define COLON using CREATE (and these other words.) */
  defword ":",1,,COLON,IMMEDIATE
  .dword WORD                  // Get the name of the new word
  .dword CREATE                // Create the header
  .dword LIT, DOCOL, COMMA     // Append docol
  .dword LATEST, FETCH, HIDDEN // Fetch the latest word, make it hidden
  .dword RBRAC                 // Compile mode
  .dword EXIT                  // Return from function.

  /* SEMICOLON is also simple. */
  defword ";",1,F_IMMED,SEMICOLON,COLON
  .dword LIT, EXIT, COMMA        // Append EXIT
  .dword LATEST, FETCH, HIDDEN   // Toggle hidden flag to make it unhidden
  .dword LBRAC                   // Go back to immediate mode
  .dword EXIT

  defword "HIDE",4,,HIDE,SEMICOLON
  .dword WORD      // Get the word
  .dword FIND      // Find that word
  .dword HIDDEN    // Set it to hidden
  .dword EXIT

  /* ' Copies the next codeword to the stack */
  defcode "'",1,,TICK,HIDE
  ld t0, (gp)
  addi gp, gp, 8
  addi sp, sp, -8
  sd t0, (sp)
  NEXT

  defcode "BRANCH",6,,BRANCH,TICK
  ld t0, (gp)
  add gp, gp, t0
  NEXT

  defcode "0BRANCH",7,,ZBRANCH,BRANCH
  ld a0, (sp)
  addi sp, sp, 8
  beqz a0, code_BRANCH
  addi gp, gp, 8
  NEXT

  defcode "LITSTRING",9,,LITSTRING,ZBRANCH
  ld a0, (gp)     // Get the length of the string
  addi gp, gp, 8  // Move gp forward
    /*ld a1, (gp) */

  addi sp, sp, -16
  sd a0, (sp)
  sd gp, 8(sp)

  add gp, gp, a0  // Skip the literal string
  addi gp, gp, 7  // Advance to the next 8-byte boundary
  and gp, gp, ~7
  NEXT

  defcode "TELL",4,,TELL,LITSTRING
  ld a0, (sp)  // length of string
  ld a1, 8(sp) // address of string
  addi sp, sp, 16
  SAVECALL _TELL
  NEXT

_TELL:
  la t0, serial_port_addr /* _address_ of the serial_port_addr variable */
  ld t0, (t0)             /* _value_ of the serial_port_addr variable (also an address) */
  mv t1, zero
write_char_loop:
  lb t1, (a1)
  addi a1, a1, 1
  sb t1, (t0)
  addi a0, a0, -1
  bnez a0, write_char_loop
  /* NMG TODO Probably put a wait in here */
  ret

  defword "QUIT",4,,QUIT,TELL
  .dword RZ,RSPSTORE // R0 RSP!, clear the return stack
  .dword INTERPRET   // interpret the next word
  .dword BRANCH,-16  // and loop forever

  defcode "CHAR",4,,CHAR,QUIT
  SAVECALL _WORD
  mv t0, zero
  lbu t0, (a1)
  addi sp, sp, -8
  sd t0, (sp)
  NEXT

  defcode "EXECUTE",7,,EXECUTE,CHAR
  ld t0, (sp)
  addi sp, sp, 8
  ld t1, (t0)
  jr t1

  /* Word to run C code. Store the state registers of fp and gp on the
	stack. Then stash sp into the scratch register. Create a context for
	the C program to run in. Jump to whatever we're supposed to jump to. */
  defcode "RUNC",4,,RUNC,EXECUTE
  addi sp, sp, -16
  sd   gp,      8(sp)
  sd   fp,       (sp)
  csrw sscratch,  sp
  NEXT

  /* There is no way this is correct. Need to graph this out */
  defcode "INTERPRET",9,,INTERPRET,RUNC
  la t0, var_LINEMODE
  ld t0, (t0)
  beqz t0, not_linemode
  SAVECALL _LINE
not_linemode:
  SAVECALL _WORD // a1 base address, a0 length
interpret_word:
  /* If we get a zero base addr here we have a problem */

  mv t0, zero
  la t1, interpret_is_lit
  li t2, 0
  sd t2, (t1)
  mv a5, a0 // length
  mv a6, a1 // base addr
  SAVECALL _FIND // a2 contains either 0 or the address of the found word
found_or_not: 
  beqz a2, not_in_dict

  /* If we don't jump, it is in the dictionary. Is it IMMEDIATE? */
  mv t3, zero
  lb t3, 8(a2) // Get name+flags
  mv a0, a2 // set up for _TCFA
  SAVECALL _TCFA // codeword pointer now in a0
  li t6, F_IMMED
  and t4, t3, t6
  bnez t4, executing // If immediate, go straight to execution
  j compile_execute_p

not_in_dict: // 1
  /* Set variable indicating lit */
  li t4, 1
  la t1, interpret_is_lit
  sd t4, (t1)

  /*  We assume its a number at this point
      Restore a0, a1 for _NUMBER
  */
  mv a0, a5
  mv a1, a6
  SAVECALL _NUMBER // a0 is unparsed characters or 0 on no-len, a2 is parsed number
  bnez a0, parse_error
  la a0, LIT // Prepare to run LIT with the word.
  mv a1, a2

compile_execute_p: // 2
  la a5, var_STATE
  ld a5, (a5)
  beqz a5, executing

  /* Compiling. Just append the word  */
  SAVECALL _COMMA
  la a4, interpret_is_lit // Check if we are compiling a literal
  ld a4, (a4)
  beqz a4, next_terminus  // If we are compiling lit, we need to also append the number.
  mv a0, a1
  SAVECALL _COMMA

next_terminus:
  NEXT

executing: // 4
  // Check if we are interpreting lit
  la a5, interpret_is_lit
  ld a5, (a5)
  bnez a5, execute_literal

  // Not a literal, execute it now
  mv t0, a0
  ld t1, (t0)
  jr t1

execute_literal: // 5
  addi sp, sp, -8
  sd a1, (sp)
  NEXT

parse_error: // 6

  .data
  .align 3
interpret_is_lit:
  .int 0

  .align 8
serial_port_addr:
  .dword 0x10000000

  .set INITIAL_DATA_SEGMENT_SIZE,1024*1024
  .set INITIAL_PARAM_STACK_SIZE,65536
	.set RETURN_STACK_SIZE,8192

	.bss
  /* param stack ends here */
  .space INITIAL_PARAM_STACK_SIZE
  .global param_stack_top
param_stack_top:
  /* param stack starts here */

  /* Forth return stack ends here */
  .align 12 /* Align to 4096 bytes, page size? */
return_stack:
  .space RETURN_STACK_SIZE
  .globl return_stack_top
return_stack_top:
  /* Forth return stack starts here */
  .dword 0x0

  .section .end.data
  .align 12 /* Page aligned */
  /* The data segment where definitions go */
  .globl data_segment_start
