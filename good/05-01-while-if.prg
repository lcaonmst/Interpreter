// Correct usage of if and while.
// i = 10, cnt = 5

int main() {
  int i = 0;
  int cnt = 0;
  while (i < 10) {
    if (i % 2 == 0) {
      cnt++;
    }
    i++;
  }
  printInt(i);
  printInt(cnt);
  return 0;
}
