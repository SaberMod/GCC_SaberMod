/* Test if -mno-copyrelocs does the right thing. */
/* { dg-do compile } */
/* { dg-options "-O2 -fpie -mno-copyrelocs" } */

extern int glob_a;

int foo ()
{
  return glob_a;
}

/* glob_a should always be accessed via GOT  */ 
/* { dg-final { scan-assembler "glob_a\\@GOT" { target { x86_64-*-* } } } } */
