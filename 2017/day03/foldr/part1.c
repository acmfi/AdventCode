#include <stdio.h>
#include <stdlib.h>

typedef struct vec_t {
  int x;
  int y;
} vec;

#define pvec(s,v) printf ("%s: (%d, %d)\n", s, v.x, v.y)
#define pow(n) ((n) * (n))
#define abs(n) ((n) < 0 ? -(n) : (n))

#define INPUT 289326

int calculate(int n);

int main(int argc, char **argv) {
  int n;
  int i;
  
  argc--;
  argv++;
  if (argc == 0) {
    n = INPUT;
    printf ("Result: %d\n", calculate (n));
  } else {
    for(i = 0; i < argc; i++)
      printf ("Result: %d\n", calculate(atoi(argv[i])));
  }
  
  return 0;
}

int calculate(int n) {
  if (n == 1) return 0;
  int i = 1;
  while (pow (i) < n) i+=2;
  if (pow (i) == n) return (i - 2) * 2;
  int prev = i - 2;

  int j = 0;
  while (pow(prev) + j*(i-1) < n) j++;
  vec corner;
  int x;
  x = pow(prev) + j*(i-1) - n;
  switch (j) {
  case 1:
    corner.x = i / 2;
    corner.y = -i / 2 + x;
    break;
  case 2:
    corner.x = -i / 2 + x;
    corner.y = -i / 2;
    break;
  case 3:
    corner.x = -i / 2;
    corner.y = i / 2 - x;
    break;
  case 4:
    corner.x = i / 2 - x;
    corner.y = i / 2;
    break;
  }
  
  return abs(corner.x) + abs(corner.y);
}
