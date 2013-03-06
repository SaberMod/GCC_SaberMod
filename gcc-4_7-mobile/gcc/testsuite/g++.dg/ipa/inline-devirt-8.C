/* Verify that the inliner makes good decisions and the example
   is optimized to 4 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }

#include <stdio.h>

template<class TOutput, typename TInput> class Factory
{
public:
	virtual TOutput* createFrom(TInput) = 0;
	virtual ~Factory() {};
};

class Calculable
{
public:
	virtual unsigned char calculate() = 0;
	virtual ~Calculable() {};
};

class X : public Calculable
{
public:
	virtual unsigned char calculate() { return 1; }
};

class Y : public Calculable
{
public:
	virtual unsigned char calculate() { throw; }
};

enum Letter
{
	LetterX,
	LetterY
};

class CalculableFactory : Factory<Calculable, Letter>
{
public:
      virtual Calculable* createFrom(Letter letter)
      {
		switch(letter)
		{
			case LetterX: return new X();
			default: return new Y();
		}
      }
};


static void print(Calculable* c)
{
	printf("+1: %d\n", c->calculate() + 1);
}

int main()
{
	CalculableFactory calcuableFactory;
	Calculable* calculable = calcuableFactory.createFrom(LetterX);

	print(calculable);

	delete calculable;

	return 0;
}

// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 2\\);" "optimized" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "optimized" } }
