// Correct usage of nested while and if..else.
// res = 2 + 3 + ... + n

int main() {
  int n = 6;
  int res = -1;
  if (n > 0) {
    int i = 0;
    while (i < n) {
      i++;
      int j = 0;
      while (j < i) {
        j++;
        res++;
      }
    }
  }
  else {
    res = 0;
  }
  printInt(res);
  return 0;
}
