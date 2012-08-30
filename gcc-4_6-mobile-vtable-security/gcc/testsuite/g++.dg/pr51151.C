/* { dg-do compile } */
/* { dg-options "-Woverflow" } */
template<typename T> class C {
public:
  void f() {
    m = c2 + 1;
  }
  static const long unsigned int c1 = 7;
  static const int c2 = c1 - 1;
  int m;
};
template class C<int>;
