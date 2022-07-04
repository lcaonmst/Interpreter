int main() {
  int N = 30;
  boolean[31] notPrime;
  notPrime[0] = true;
  notPrime[1] = true;
  int i = 2;
  while (i <= N) {
    if (notPrime[i]) {
      i++;
      continue;
    }
    printInt(i);
    int j = i * 2;
    while (j <= N) {
      notPrime[j] = true;
      j = j + i;
    }
    i++;
  }

  i = 0;
  while (i <= N) {
    printString("Is prime a number");
    printInt(i);
    printBoolean(!notPrime[i]);
    i++;
  }
  return 0;
}
