// Correct usage of diferent types functions

string hello() {
  return "Hello world!";
}

boolean is_non_zero(int a, int b, int &c) {
  return a != 0 || b != 0 || c != 0;
}

int main() {
  string s = hello();
  int a = 0, b = 0, c = 1;
  boolean p = is_non_zero(a, b, c);
  printBoolean(p);
  return 0;
}
