/* Verify that the inliner makes good decisions and the example
   is optimized to 4 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }

#include <stdio.h>

typedef unsigned char(*Calculable)(void);
typedef Calculable(*CalculateStrategy)(void);

unsigned char one() { return 1; }
Calculable oneStrategy() { return one; }
unsigned char two() { return 2; }
Calculable twoStrategy() { return two; }

static void print(CalculateStrategy calculateStrategy)
{
	printf("%d\n", calculateStrategy()());
	printf("+1: %d\n", calculateStrategy()() + 1);
}

int main()
{
	print(oneStrategy);
	print(twoStrategy);

	return 0;
}

// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 1\\);" "optimized" { xfail *-*-* } } }
// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 2\\);" "optimized" { xfail *-*-* } } }
// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 2\\);" "optimized" { xfail *-*-* } } }
// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 3\\);" "optimized" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "optimized" } }
