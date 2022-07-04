// Correct usage of references.
// a = 0, b = 1

int increment(int a, int &b) {
  a++;
  b++;
  return 0;
}

int main() {
  int a = 0, b = 0;
  increment(a, b);
  printInt(a);
  printInt(b);
  return 0;
}
