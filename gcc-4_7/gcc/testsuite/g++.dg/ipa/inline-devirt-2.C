/* Verify that the inliner makes good decisions and the example
   is optimized to 4 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }

#include <stdio.h>

class Calculable
{
public:
	virtual unsigned char calculate() = 0;
	virtual ~Calculable() {}
};

class X : public Calculable
{
public:
	virtual unsigned char calculate() { return 1; }
};

class Y : public Calculable
{
public:
	unsigned char calculate() { return 2; }
};

static void print(Calculable& c)
{
	printf("%d\n", c.calculate());
	printf("+1: %d\n", c.calculate() + 1);
}

int main()
{
	X x;
	Y y;

	print(x);
	print(y);

	return 0;

}

// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 1\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 2\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 2\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 3\\);" "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
