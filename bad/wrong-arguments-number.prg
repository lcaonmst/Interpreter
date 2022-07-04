// Wrong arguments number delivered to function on line 9 - error.

int fun(int a, int b) {
  return a + b;
}

int main() {
  int a = 1, b = 2, c = 3;
  int d = fun(a, b, c);
  return -1;
}

