/*
 * branch-test.c - Simple branch test program for SimpleScalar
 */

#include <stdio.h>

int main(void)
{
  int i;
  int count = 10;
  int sum = 0;

  /* Loop with conditional branches */
  for (i = 0; i < count; i++)
  {
    if (i % 2 == 0)
    {
      /* Even case */
      sum += i;
    }
    else
    {
      /* Odd case */
      sum += i * 2;
    }
  }

  /* Nested branches */
  if (sum > 50)
  {
    if (sum % 2 == 0)
    {
      sum = sum / 2;
    }
    else
    {
      sum = sum * 2;
    }
  }
  else
  {
    if (sum % 2 == 0)
    {
      sum = sum + 10;
    }
    else
    {
      sum = sum + 5;
    }
  }

  /* Switch statement for more branches */
  switch (sum % 4)
  {
  case 0:
    sum += 1;
    break;
  case 1:
    sum += 2;
    break;
  case 2:
    sum += 3;
    break;
  default:
    sum += 4;
    break;
  }

  printf("Final sum: %d\n", sum);

  return sum;
}