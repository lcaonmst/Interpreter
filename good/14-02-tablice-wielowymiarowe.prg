// Multidimensional array behaviours.
// res = 4

int fun(int[2][2] arr) {
  arr[0][1] = 1;
  arr[1][0] = 1;
  return arr[0][0] + arr[1][1];
}

int main() {
  int[2][2][2] arr;
  arr[0][0][0] = 1;
  arr[0][1][1] = arr[0][0][0];
  int res = fun(arr[0]) + arr[0][1][0] + arr[0][0][1];
  printInt(res);
  return 0;
} 
