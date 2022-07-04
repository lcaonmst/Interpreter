// Partial tuples not allowed on the right of assigment - error.

int main() {
  (int, int) a;
  a = (5, _);
  return -1;
}
