// Literal 1 cannot be treated as variable on line 3 - error.

int fun(int &a) {
  a++;
  return 0;
}

int main() {
  int r = fun(1);
  return -1;
}
