/* Verify that the inliner makes good decisions and the example
   is optimized to 1 printf() in main().  */
// { dg-do compile }
// { dg-options "-O2 -std=c++0x -fdump-tree-optimized"  }

#include <stdio.h>
#include <memory>

using namespace std;

template<class TOutput, typename TInput> class Factory
{
public:
	virtual shared_ptr<TOutput> createFrom(TInput) = 0;
	virtual ~Factory() {};
};

class Calculable
{
public:
	virtual unsigned char calculate() = 0;
	virtual ~Calculable() {};
};

enum LetterType
{
	LetterX,
	LetterY
};

class Letter
{
public:
	Letter(LetterType type) : _type(type) {}
	virtual ~Letter() {}

protected:
	LetterType _type;
};

class X : public Letter, public Calculable
{
public:
	X() : Letter(LetterX) {}
	virtual unsigned char calculate() { return 1; }
};

class Y : public Letter, public Calculable
{
public:
	Y() : Letter(LetterY) {}
	virtual unsigned char calculate() { throw; }
};

class CalculableFactory : Factory<Calculable, LetterType>
{
public:
      virtual shared_ptr<Calculable> createFrom(LetterType letter)
      {
		switch(letter)
		{
			case LetterX: return make_shared<X>();
			default: return make_shared<Y>();
		}
      }
};


static void print(Calculable& c)
{
	printf("+1: %d\n", c.calculate() + 1);
}

int main()
{
	CalculableFactory calcuableFactory;
	auto calculable = calcuableFactory.createFrom(LetterX);

	print(*calculable);

	return 0;
}

// { dg-final { scan-tree-dump "printf \\(\"\\+1: %d\\\\n\", 2\\);" "optimized" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "optimized" } }
