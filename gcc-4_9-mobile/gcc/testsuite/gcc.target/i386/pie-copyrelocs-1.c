/* Test if -mcopyrelocs does the right thing. */
/* { dg-do compile } */
/* { dg-options "-O2 -fpie -mcopyrelocs" } */

extern int glob_a;

int foo ()
{
  return glob_a;
}

/* glob_a should never be accessed with a GOTPCREL  */ 
/* { dg-final { scan-assembler-not "glob_a\\@GOTPCREL" { target { x86_64-*-* } } } } */
