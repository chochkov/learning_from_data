#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_SURFACE 1e-14
#define LEARNING_RATE (double)1/10

#define INIT_U (double)1
#define INIT_V (double)1

struct Point {
  double u;
  double v;
};

struct Point *initPoint(double u, double v)
{
  struct Point *result = (struct Point *)malloc(sizeof(struct Point));

  result->u = u;
  result->v = v;

  return result;
}

double surface(struct Point *p)
{
  return pow((p->u * exp(p->v) - 2 * p->v * exp(-p->u)), 2);
}

double dv(struct Point *p)
{
  return 2*((p->u * exp(p->v) - 2 * exp(-p->u)) * (p->u * exp(p->v) - 2 * p->v * exp(-p->u)));
}

double du(struct Point *p)
{
  return 2*((exp(p->v) + 2 * p->v * exp(-p->u)) * (p->u * exp(p->v) - 2 * p->v * exp(-p->u)));
}

void coordinateDescend(struct Point *p)
{
  p->u -= LEARNING_RATE * du(p);
  p->v -= LEARNING_RATE * dv(p);
}

void descend(struct Point *p)
{
  double new_u = p->u - LEARNING_RATE * du(p);
  double new_v = p->v - LEARNING_RATE * dv(p);

  p->u = new_u;
  p->v = new_v;
}

void printPoint(struct Point *p)
{
  printf("Point: (%e, %e). Surface: %e. Rotor: (%e %e)\n", p->u, p->v, surface(p), du(p), dv(p));
}

int main(int argc, const char *argv[])
{
  struct Point *point = initPoint(INIT_U, INIT_V);

  int i = 0;
  while (surface(point) > MAX_SURFACE && i < 15)
  {
    i++;
    printPoint(point);
    /* descend(point); */
    coordinateDescend(point);
  }
  printf("%d\n", i);

  free(point);

  return 0;
}
