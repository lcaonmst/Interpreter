// Correct usage of nested function definitions.
// p = true

int main() {
  
  int x = 0;
  boolean is_zero() {
    return x == 0;
  }
  x++;
  boolean p = is_zero();
  printBoolean(p);
  return 0;
}
