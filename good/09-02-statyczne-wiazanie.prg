// Correct usage of nested function definitions.
// p = true

int main() {
  
  int x = 0;
  boolean is_zero() {
    printInt(x);
    int x = 5;
    printInt(x);
    return x == 5;
  }
  x++;
  printInt(x);
  boolean is_one() {
    return x == 1;
  }
  boolean p = is_zero() && is_one();
  printInt(x);
  printBoolean(p);
  return 0;
}
