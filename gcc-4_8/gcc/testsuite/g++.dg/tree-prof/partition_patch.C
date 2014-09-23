// Check if patching works with function splitting.
// { dg-do compile { target x86_64-*-* } }
// { dg-require-effective-target freorder }
// { dg-options "-O2 -fnon-call-exceptions -freorder-blocks-and-partition -mpatch-functions-for-instrumentation -fno-optimize-sibling-calls " }

int k;

int
main ()
{
  try
  {
    if (k)
      throw 6;
  }
  catch (...)
  {
  }
}
