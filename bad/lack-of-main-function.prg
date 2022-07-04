// Lack of main function - error

int fun1(int a) {
  return 2 * a;
}

int fun2(int a) {
  if (a <= 0) {
    return 1;
  }
  else {
    return a * fun(a - 1);
  }
}
