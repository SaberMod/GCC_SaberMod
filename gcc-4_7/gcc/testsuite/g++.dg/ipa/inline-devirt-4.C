/* Verify that the inliner makes good decisions and the example
   is optimized to 2 printf()s in main().  */
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }

#include <stdint.h>
#include <stdio.h>

class String
{
public:
  virtual uint64_t length() const = 0;
  virtual char get(uint64_t index) const = 0;
  virtual void set(uint64_t index, char value) = 0;
  virtual char& operator[] (uint64_t value) = 0;
  virtual ~String() {};
};

template<uint64_t size> class FixedString : public String
{
private:
  char contents[size + 1];

public:
  virtual uint64_t length() const { return size; }
  virtual char get(uint64_t index) const { return contents[index]; }
  virtual void set(uint64_t index, char value) { contents[index] = value; }
  virtual char& operator[] (uint64_t index) { return contents[index]; }

  FixedString() { contents[size] = '\0'; }
};

void print_length (const String& string)
{
  for (uint64_t i = 0; i < string.length(); i++)
    {
      printf("%d\n", string.get(i));
    }
}

int main()
{
  FixedString<2> empty;
  empty[0] = 'a';
  empty[1] = 'b';

  print_length(empty);

  return 0;
}

// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 97\\);" "optimized" { xfail *-*-* } } }
// { dg-final { scan-tree-dump "printf \\(\"%d\\\\n\", 98\\);" "optimized" { xfail *-*-* } } }
// { dg-final { cleanup-tree-dump "optimized" } }
