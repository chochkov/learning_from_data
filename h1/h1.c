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

double runif()
{
  return (double)random() / (double)RAND_MAX * 2 - 1;
}

Line *randomLine()
{
  Line *line = (Line *)malloc(sizeof(Line));

  line->x1 = runif();
  line->y1 = runif();
  line->x2 = runif();
  line->y2 = runif();

  // In fact only X == Y should be checked and not the coordinate-based check
  // here, but we dont want parallel lines to the ordinate for this code too.
  if (line->x1 == line->x2) {
    free(line);
    return randomLine();
  }

  return line;
}

double thresholdX2(double x, Line *line)
{
  return (line->y1 - line->y2) / (line->x1 - line->x2) * (x - line->x1) + line->y1;
}

Datum *dataset(Line *line)
{
  Datum *result = (Datum *)malloc(sizeof(Datum) * N);

  int i;
  for (i = 0; i < N; i++) {
    Datum point;

    point.x1 = runif();
    point.x2 = runif();
    point.y_hat = 0;

    if (point.x2 <= thresholdX2(point.x1, line)) {
      point.y = -1;
    } else {
      point.y = 1;
    }

    result[i] = point;
  }

  return result;
}

Weight *initWeights()
{
  Weight *result = (Weight *)malloc(sizeof(Weight));

  result->w0 = 0;
  result->w1 = 0;
  result->w2 = 0;

  return result;
}

Datum *misclassified(Datum *X)
{
  int size = 0;
  int i;
  for (i = 0; i < N; i++) {
    if (X[i].y != X[i].y_hat) size ++;
  }

  Datum *result = (Datum *)malloc(sizeof(Datum) * size);
  for (i = 0; i < N; i++) {
    if (X[i].y != X[i].y_hat) {
      result[i] = X[i];
      printf("have %d want %d. Points: %f, %f\n", X[i].y, X[i].y_hat, X[i].x1, X[i].x2);
    }
  }

  return result;
}

void adjustWeights(Weight *w, Datum *m)
{
  int size = 0;
  int i;
  for (i = 0; i < N; i++) {
    if (m[i].y != m[i].y_hat) size ++;
  }

  int index = random() % size;

  printf("\n");
  printf("Missclassified: %d\n", size);
  printf("want: %d, have: %d\n", m[index].y, m[index].y_hat);
  printf("x1: %f, x2: %f\n", m[index].x1, m[index].x2);

  w->w0 += m[index].y * 1;
  w->w1 += m[index].x1 * m[index].y;
  w->w2 += m[index].x2 * m[index].y;
}

int iterations(Datum *X)
{
  int result = 0;
  Weight *w = initWeights();

  do {
    int i;
    for (i = 0; i < N; i++) {
      double q = w->w0 + w->w1 * X[i].x1 + w->w2 * X[i].x2;
      printf("quantity %f\n", q);
      X[i].y_hat = q > 0 ? -1 : 1;
    }
    result ++;

    Datum *m = misclassified(X);

    int size = 0;
    int s = 0;
    for (s = 0; s < N; s++) {
      if (m[s].y != m[s].y_hat) size ++;
    }
    if (size == 0) break;

    adjustWeights(w, m);
    free(m);
    printf("NEW WEIGHTS\n");
    printf("%f\n", w->w0);
    printf("%f\n", w->w1);
    printf("%f\n", w->w2);
  } while (true);

  free(w);
  return result;
}

int main(void)
{
  srandom(time(NULL));

  int iterations_total = 0;

  int j;
  for (j = 0; j < SIMULATIONS; j++) {
    Line  *line   = randomLine();

    Datum *X = dataset(line);
    /* printf("%f\n", (double)sizeof(X)/(double)sizeof(Datum)); */

    printf("X1 = %f, Y1 = %f, X2 = %f, Y2 = %f\n", line->x1, line->y1, line->x2, line->y2);

    printf("X1       X2       thrshld  Y\n");
    int i;
    for (i = 0; i < N; i++) {
      printf("%f %f %f %d\n", X[i].x1, X[i].x2, thresholdX2(X[i].x1, line), X[i].y);
    }
    /* iterations_total += iterations(X); */

    free(line);
    free(X);
  }

  printf("Average number of iterations for %d simulations: %f\n", SIMULATIONS, (double)iterations_total/SIMULATIONS);
  return 0;
}
