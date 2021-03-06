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

  @8192
  D=D+A
  @n
  M=D //set n = 256 rows * 32 words

(REFRESH)
  //initialise i = 0
  @i
  M=0

  @SCREEN
  D=A
  @address
  M=D //set address to base address of SCREEN Memory Map

  @KBD
  D=M
  @EMPTYDISPLAY
  D;JEQ // if (KBD == 0) Empty Display
  @FILLDISPLAY
  D;JGT // if (KBD > 0) Fill Display

(EMPTYDISPLAY)
  @fillorempty
  M=0
  @LOOP
  0;JEQ

(FILLDISPLAY)
  @fillorempty
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

  //Fill OR Empty address
  @fillorempty
  D=M
  @address
  A=M
  M=D

  // add 1 to address for next iteration
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
  0;JMP
