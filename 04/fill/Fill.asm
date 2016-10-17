// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(REFRESH)
  @8192
  D=D+A
  @n
  M=D //set n = 256 rows * 32 words

  //initialise i = 0
  @i
  M=0

  //reset address
  @address
  M=0

  @SCREEN
  D=A
  @address
  M=D //set address to base address of SCREEN Memory Map

  @KBD
  D=M
  @EMPTY
  D;JEQ // Empty Display
  @FILL
  D;JGT // Fill Display

(EMPTY)
  @fillval
  M=0
  @LOOP
  0;JEQ

(FILL)
  @fillval
  M=-1
  @LOOP
  0;JEQ

(LOOP)
  //if (i > n) GOTO REFRESH
  @i
  D=M
  @n
  D=D-M
  @REFRESH
  D;JEQ

  @fillval
  D=M
  @address
  A=M
  M=D
  @address
  D=M

  @1
  D=D+A
  @address
  M=D

  //i++
  @i
  M=M+1

  @LOOP
  D;JGT
