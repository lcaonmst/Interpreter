// Correct usage of literals, arythmetic and comparisions.
// a = 2, b = 1, c = true, d = false, e = 1, f = true, g = -3, h = true

int main() {
  int a = 2;
  int b = 1 + a + a * a - 3 * a; 
  boolean c = b == 0 || b != 0;
  boolean d = c && !c;
  int e = 15 / a % 3;
  boolean f = (b < 0) || (e < 0);
  int g = a + (-5);
  boolean h = a <= 2;
  
  printInt(a);
  printInt(b);
  printBoolean(c);
  printBoolean(d);
  printInt(e);
  printBoolean(f);
  printInt(g);
  printBoolean(h);
  
  return 0;
}
  
