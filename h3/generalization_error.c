#include <stdio.h>
#include <math.h>

float bound(M, N)
{
  return 2*M*exp(-2*0.05*0.05*N);
}

int main(int argc, const char *argv[])
{
  printf("Problem 1\n");
  printf("%f\n", bound(1, 500));
  printf("%f\n", bound(1, 1000));
  printf("%f\n", bound(1, 1500));
  printf("%f\n", bound(1, 2000));

  printf("Problem 2\n");
  printf("%f\n", bound(10, 500));
  printf("%f\n", bound(10, 1000));
  printf("%f\n", bound(10, 1500));
  printf("%f\n", bound(10, 2000));

  printf("Problem 3\n");
  printf("%f\n", bound(100, 500));
  printf("%f\n", bound(100, 1000));
  printf("%f\n", bound(100, 1500));
  printf("%f\n", bound(100, 2000));

  return 0;
}
