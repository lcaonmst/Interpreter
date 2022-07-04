// Correct usage of function and recursion.
// a = 1, b = 120

int silnia(int n) {
  if (n <= 0) {
    return 1;
  }
  else {
    return n * silnia(n - 1);
  }
}

int main() {
  int a = silnia(-1);
  int b = silnia(a + 4);
  printInt(a);
  printInt(b);
  return 0;
}
