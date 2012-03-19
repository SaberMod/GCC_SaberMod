/* Verify if call-graph profile sections are created
   with -freorder-functions=. */
/* { dg-options "-O2 -freorder-functions=callgraph -ffunction-sections --save-temps" } */
/* { dg-require-section-exclude "" } */

int __attribute__ ((noinline))
foo ()
{
  return 1;
}

int __attribute__ ((noinline))
bar ()
{
  return 0;
}

int main ()
{
  int sum;
  for (int i = 0; i< 1000; i++)
    {
      sum = foo () + bar();
    }
  return sum * bar ();
}

/* { dg-final-use { scan-assembler "\.gnu\.callgraph\.text\.main" } } */
/* { dg-final-use { scan-assembler "\.string \"1000\"" } } */
/* { dg-final-use { cleanup-saved-temps } }  */
