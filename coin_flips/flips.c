#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>


int * simulate(int goal)
{
  int n;
  static int t[2];
  t[0] = 0;
  t[1] = 0;
  while (1)
    {
      t[1]++;
      for (n = 1; n <= goal; n++)
	{
	  if (rand()%2) break;
	}
      if (goal < n) return t;
      else t[0] += n;
    }
}


int main()
{
  int i, *s;
  int limit = 100000000;
  srand(time(NULL));

  long attempts = 0;
  long flips = 0;
  for (i = 0; i < limit; i++)
    {
      s = simulate(10);
      flips += s[0];
      attempts += s[1];
      //printf("Ten heads in after %d flips in %d attempts\n", s[0], s[1]); 
    }
  printf("Average of %f flips and %f attempts\n",
	 (float) flips / limit,
	 (float) attempts / limit);
}
