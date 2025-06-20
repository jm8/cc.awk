int func() {
  int a;
  int b;
  int c;
  int d;
  int e;
  a = 1;
  b = 2;
  c = 3;
  b = a;
  c = b;
  d = c;
  e = c;
  d = e;
  c = d;
  return c;
}
