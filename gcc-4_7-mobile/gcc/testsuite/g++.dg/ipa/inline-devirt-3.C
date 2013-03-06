/* Verify that the inliner makes good decisions and the example
   is optimized to 3 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }

#include <stdio.h>

class Calculable
{
public:
	virtual unsigned char calculate() const = 0;
	virtual ~Calculable() {}
};

class X : public Calculable
{
public:
	virtual unsigned char calculate() const { return 0; }
};

class Y : public Calculable
{
public:
	virtual unsigned char calculate() const { return 3; }
};

static void print(const Calculable& c)
{
	for (int i = 0; i < c.calculate(); i++)
	{
		printf("%d\n", c.calculate());
	}
}

int main()
{
	X x;
	Y y;

	print(x);
	print(y);

	return 0;
}

// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 3\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 3\\);" "optimized" } }
// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 3\\);" "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
