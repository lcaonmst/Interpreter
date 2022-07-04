// Tuple branches not allowed on both lvalue and rvalue - error.

int main() {
  int a, b;
  (a, b) = (1, 2);
  return -1;
}
