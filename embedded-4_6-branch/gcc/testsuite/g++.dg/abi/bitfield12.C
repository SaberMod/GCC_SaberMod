// { dg-options "-Wabi -fabi-version=1 -fno-strict-volatile-bitfields" }

struct S { // { dg-warning "ABI" }
  char c : 1024; // { dg-warning "width" }
};
