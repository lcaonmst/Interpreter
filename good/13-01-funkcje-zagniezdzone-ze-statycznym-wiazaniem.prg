// Correct usage of static binding.
// w = 2

int main() {
  int x = 15;
  int fun1() {
    int res = 0;
    if (x == 15) {
      res++;
    }
    x++;
    int fun2() {
      int res2 = 0;
      if (x == 16) {
        res2 = 1;
      }
      return res2;
    }
    x++;
    res = res + fun2();
    return res;
  }
  x++;
  int w = fun1();
  printInt(w);
  return 0;
}
