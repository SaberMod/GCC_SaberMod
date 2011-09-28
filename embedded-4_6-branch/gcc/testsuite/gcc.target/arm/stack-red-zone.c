/* No stack red zone.  PR38644.  */
/* { dg-options "-mthumb -O2" } */
/* { dg-final { scan-assembler "ldrb\[^\n\]*\\n\[\t \]*add\[\t \]*sp\[^\n\]*\\n\[\t \]*@\[^\n\]*\\n\[\t \]*pop" } } */
/* { dg-require-effective-target arm_thumb1_ok } */

extern int doStreamReadBlock (int *, char *, int size, int);

char readStream (int *s)
{
       char c = 0;
       doStreamReadBlock (s, &c, 1, *s);
       return c;
}
