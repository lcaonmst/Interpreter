// Some examples of incorrect statements which produce interpreter error.
// a) Dividing by zero, b) incorrect cast, c) non-existing variable.

int main() {
  int a = 6 / 0;
  int b = "Hello World!";
  int c = d + 5;
  return -1;
}
