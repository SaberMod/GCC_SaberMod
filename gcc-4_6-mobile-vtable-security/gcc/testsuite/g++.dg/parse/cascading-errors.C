// { dg-do compile }
// { dg-options "-Wall -Werror" }
// a.cc:11:3: error: 'B::B' names the constructor, not the type
// a.cc:11:8: error: expected ';' before 'f'
// a.cc:11:13: error: statement cannot resolve address of overloaded function
// a.cc:18:1: error: control reaches end of non-void function [-Werror=return-type]
 

// After a parsing error, the IL ends up in an inconsistent state.
// In this case, the parser never adds destructor calls, which causes
// B::~B to never be called, triggering the error 'control reaches end
// of non-void function' from the middle end. This second error is
// bogus and only confuses the user.

int bar(int);

struct B {
    B(int);
    ~B() __attribute__((noreturn));
};


int foo(void)
{
  B::B f(10);	// { dg-error "names the constructor" }
		// { dg-error "expected ';'" "" { target *-*-* } 25 }
		// { dg-error "cannot resolve" "" { target *-*-* } 25 }
  return 0;
}

int bar(int j)
{
  B(10);
}
