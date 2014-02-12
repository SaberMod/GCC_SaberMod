/* Check if the gold function reordering plugin reorders split functions.
   Check if foo is split and the cold section of foo is not next to its hot
   section*/
/* { dg-require-section-exclude "" } */
/* { dg-require-linker-function-reordering-plugin "" } */
/* { dg-require-effective-target freorder } */
/* { dg-options "-O2 -freorder-functions=callgraph -ffunction-sections -freorder-blocks-and-partition --save-temps -Wl,-plugin-opt,file=linker.dump" } */


#define SIZE 10000

const char *sarr[SIZE];
const char *buf_hot;
const char *buf_cold;

__attribute__ ((noinline))
int bar (int *arg)
{
  (*arg)++;
  return 0;
}

__attribute__((noinline))
void 
foo (int path)
{
  int i;
  bar (&path);
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

/* { dg-final-use { scan-assembler "\.string \"ColdWeight 0\"" } } */
/* { dg-final-use { scan-assembler "\.section.*\.text\.hot\._Z3fooi" } } */
/* { dg-final-use { scan-assembler "\.section.*\.text\.unlikely\._Z3fooi" } } */
/* { dg-final-use { cleanup-saved-temps } }  */
/* { dg-final-use { scan-file linker.dump "Callgraph group : _Z3barPi _Z3fooi main\n" } }  */
/* { dg-final-use { scan-file linker.dump "\.text\.unlikely\._Z3fooi .* split = 1" } } */
/* { dg-final-use { scan-file linker.dump "\.text\.unlikely\._Z3fooi\[^\n\]*\n\.text\.unlikely\._Z3barPi\[^\n\]*\n" } }  */
/* { dg-final-use { scan-file linker.dump "\.text\._Z3barPi\[^\n\]*\n\.text\.hot\._Z3fooi" } }  */
/* { dg-final-use { remove-build-file "linker.dump" } } */
