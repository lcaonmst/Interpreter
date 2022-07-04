// Correct usage of nested functions.
// r = 15

int main() {
  int double_factorial(int n) {
    int fun(int k) {
       return double_factorial(k - 1);
    }
    if (n <= 0) {
      return 1;
    }
    return n * fun(n - 1);
  }
  int r = double_factorial(5);
  printInt(r);
  return 0;
}
