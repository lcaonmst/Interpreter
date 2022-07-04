// 'break' not allowed on the outside of loop - error.

int main() {
  int i = 0;
  while (i < 7) {
    i++;
  }
  break;
  return -1;
}
