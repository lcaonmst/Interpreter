// Static binding works only on identificators, the content of array is dynamic.

int main() {
  int[5] tab1, tab2;
  tab1[0] = 1;
  int fun() {
    return tab1[0];
  }
  tab1 = tab2;		// fun sees old tab1.
  printInt(fun());
  tab2[1]++;
  int fun2() {
    return tab2[1];
  }
  tab2[1]++;
  printInt(fun2());	// fun2 sees tab[1] incremented twice.
  
  
  return 0;
}
