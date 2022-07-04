// Incorrect cast boolean to integer on line 8 - error.

int fun(int a, int b) {
  return a + b;
}

int main() {
  int r = fun(2, 0 == 1);
  return -1;
}
