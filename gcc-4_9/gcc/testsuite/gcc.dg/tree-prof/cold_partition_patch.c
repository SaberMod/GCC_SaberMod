/* Check if patching works with function splitting. */
/* { dg-do compile { target x86_64-*-* } } */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-blocks-and-partition -save-temps -mpatch-functions-for-instrumentation -fno-optimize-sibling-calls " } */

#define SIZE 10000

const char *sarr[SIZE];
const char *buf_hot;
const char *buf_cold;

__attribute__((noinline))
void 
foo (int path)
{
  int i;
  if (path)
    {
      for (i = 0; i < SIZE; i++)
	sarr[i] = buf_hot;
    }
  else
    {
      for (i = 0; i < SIZE; i++)
	sarr[i] = buf_cold;
    }
}

int
main (int argc, char *argv[])
{
  buf_hot =  "hello";
  buf_cold = "world";
  foo (argc);
  return 0;
}

/* { dg-final-use { cleanup-saved-temps } } */
