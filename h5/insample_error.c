#include <stdio.h>
#include <math.h>

#define SIGMA (1/10.0)
#define D 8

float error(int n)
{
  return pow(SIGMA, 2) * (1 - (D+1) / (float)n);
}

int main(int argc, const char *argv[])
{
  int opts[5] = {10,25,100,500,1000};

  int i;
  for (i = 0; i < 5; i++) {
    printf("Error for size %d: %f\n", opts[i], error(opts[i]));
  }

  return 0;
}
