// Nesting tuples not allowed in one declaration on line 5.

int main() {
  ((int, int), int) a;
  a = ((1, 2), 1);
  return 0;
}
