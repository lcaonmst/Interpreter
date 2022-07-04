// Correct usage of continue and break;
// i = 6, res = 10

int main() {
  int res = 0;
  int i = 0;
  while (i < 9) {
    res++;
    i++;
    if (i < 3) {
      continue;
    }
    res++;
    if (i == 6) {
      break;
    }
  }
  printInt(i);
  printInt(res);
  return 0;
} 
