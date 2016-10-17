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

  @0
  D=M
  @INFINITE_LOOP
  D;JLE
  @counter
  M=D
  @SCREEN
  D=A
  @address
  M=D

(LOOP)
  @address
  A=M
  M=-1
  @address
  D=M
  @1
  D=D+A
  @address
  M=D
  @counter
  MD=M-1
  @LOOP
  D;JGT

(INFINITE_LOOP)
  @INFINITE_LOOP
  0;JMP
