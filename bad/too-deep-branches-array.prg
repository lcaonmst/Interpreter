// arr[0] is integer, not an array - error.

int main() {
  int[5] arr;
  arr[0][0] = 3;
  return -1;
}
