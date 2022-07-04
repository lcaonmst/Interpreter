// Wrong function definition order, fun1 not declared on line 4 - error.

int main() {
  int a = fun1();
  return -1;
}

int fun1() {
  return 12;
}
