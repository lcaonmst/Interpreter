// Correct usage of variables, assigments and inc/decrements.
// a = 2, b = 0, c = 0, d = true, e = 1, f = 2, g = 6

int main() {
  int a = 1 + 1;
  int b = a - 1;
  b--;
  int c = b - 1;
  c++;
  boolean d = b == c;
  int e = 1, f = a, g = 3 + 3;
  printInt(a);
  printInt(b);
  printInt(c);
  printBoolean(d);
  printInt(e);
  printInt(f);
  printInt(g);
  return 0;
}

