/*
 * branch-stall-test.c - Benchmark to test branch stalls in sim-pipe
 *
 * This benchmark contains various instruction types:
 * - Loads and stores
 * - Arithmetic operations (add, sub, mul)
 * - Branch instructions (to observe stall behavior)
 */

#include <stdio.h>

/* Function with arithmetic operations and branches */
int compute_with_branches(int a, int b)
{
  int result = 0;
  int temp = 0;

  /* Load test */
  int array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  /* Arithmetic operations */
  result = a + b;         /* Addition */
  temp = a - b;           /* Subtraction */
  result = result * temp; /* Multiplication */

  /* First conditional branch */
  if (a > b)
  {
    /* Store operation */
    array[0] = result;
    result = result + 10;
  }
  else
  {
    /* Store operation */
    array[1] = result;
    result = result - 5;
  }

  /* Load after branch */
  temp = array[2];

  /* Second conditional branch with multiple paths */
  if (result > 20)
  {
    result = result / 2; /* Division */
  }
  else if (result > 10)
  {
    result = result + temp;
  }
  else
  {
    result = result * 2;
  }

  /* Nested branches to create more branch scenarios */
  if (temp > 0)
  {
    if (result > 15)
    {
      /* Store operation */
      array[3] = result;
    }
    else
    {
      /* Store operation */
      array[4] = result;
    }
  }

  return result;
}

int main(void)
{
  int i;
  int result = 0;

  /* Loop with branch condition */
  for (i = 0; i < 5; i++)
  {
    /* Call function with branches */
    result += compute_with_branches(i, 5 - i);

    /* More branch behavior */
    if (i % 2 == 0)
    {
      result += i;
    }
    else
    {
      result -= i;
    }
  }

  printf("Final result: %d\n", result);
  return result;
}