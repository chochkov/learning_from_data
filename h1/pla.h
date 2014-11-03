#ifndef PLAH1
#define PLAH1

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

#define N 10
#define SIMULATIONS (int)2

typedef struct {
  double w0;
  double w1;
  double w2;
} Weight;

typedef struct {
  double x1;
  double x2;
  int y;
  int y_hat;
} Datum;

typedef struct {
  double x1;
  double y1;
  double x2;
  double y2;
} Line;

#endif // PLAH1
