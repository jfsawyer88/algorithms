#include <stdio.h>

// greatest common divisor
int gcd(int a, int b)
{
  int t;
  while (b)
    {
      t = b;
      b = a % b;
      a = t;
    }
  return a;
}


typedef struct
{
  int num; // numerator
  int den; // denominator
} rational;

// constructs a rational number
rational rational_construct(int num, int den)
{
  if (0 != den)
    {
      rational ratio;
      int g = gcd(num, den);
      if (g) // ensures that num and den are coprime
	{
	  num /= g;
	  den /= g;
	}
      else  // zero in standard form
	{
	  num = 0;
	  den = 1;
	}
      // ensures that denom is positive
      if (den < 0)
	{
	  num *= -1;
	  den *= -1;
	}
      ratio.num = num;
      ratio.den = den;
      return ratio;
    }
  else num/den;
}

// add two rationals
rational radd(rational a, rational b)
{
  rational ratio;
  int num = (a.num * b.den) + (b.num * a.den);
  int den = a.den * b.den;
  ratio = rational_construct(num, den);
  return ratio;
}

// subtract two rationals
rational rsub(rational a, rational b)
{
  rational ratio;
  int num = (a.num * b.den) - (b.num * a.den);
  int den = a.den * b.den;
  ratio = rational_construct(num, den);
  return ratio;
}

// multiply two rationals
rational rmul(rational a, rational b)
{
  rational ratio;
  int num = a.num * b.num;
  int den = a.den * b.den;
  ratio = rational_construct(num, den);
  return ratio;
}

// divide two rationals
rational rdiv(rational a, rational b)
{
  rational ratio;
  int num = a.num * b.den;
  int den = a.den * b.num;
  ratio = rational_construct(num, den);
  return ratio;
}

// numbers of the form a + b*sqrt(5)
typedef struct
{
  rational r1;
  rational r2;
} q_sqrt5;

q_sqrt5 q_sqrt5_construct(rational r1, rational r2)
{
  q_sqrt5 q;
  q.r1 = r1;
  q.r2 = r2;
  return q;
}

q_sqrt5 qadd(q_sqrt5 q1, q_sqrt5 q2)
{
  q_sqrt5 q;
  q.r1 = radd(q1.r1, q2.r1);
  q.r2 = radd(q1.r2, q2.r2);
  return q;
}

q_sqrt5 qsub(q_sqrt5 q1, q_sqrt5 q2)
{
  q_sqrt5 q;
  q.r1 = rsub(q1.r1, q2.r1);
  q.r2 = rsub(q1.r2, q2.r2);
  return q;
}

q_sqrt5 qmul(q_sqrt5 q1, q_sqrt5 q2)
{
  q_sqrt5 q;
  q.r1 = radd(rmul(q1.r1, q2.r1),
	    rmul(rational_construct(5, 1),
		 rmul(q1.r2, q2.r2)));
  q.r2 = radd(rmul(q1.r2, q2.r1),
	    rmul(q1.r1, q2.r2));
  return q;
}

q_sqrt5 qpow(q_sqrt5 q_in, int n)
{
  q_sqrt5 q = q_sqrt5_construct(rational_construct(1, 1),
				rational_construct(0, 1));
  while (n > 0)
    {
      q = qmul(q, q_in);
      n--;
    }
  return q;
}

void printq(q_sqrt5 q)
{
  printf("%d/%d + %d/%d*sqrt(5)\n", q.r1.num, q.r1.den, q.r2.num, q.r2.den);
}

main()
{

  // golden ratio
  q_sqrt5 phi = q_sqrt5_construct(rational_construct(1, 2),
				  rational_construct(1, 2));
  // inverse of golden ratio
  q_sqrt5 phi_inv = q_sqrt5_construct(rational_construct(-1, 2),
				      rational_construct(1, 2));

  q_sqrt5 output = qadd(qpow(phi, 2),
			qpow(phi_inv, 2));

  printq(qpow(phi, 2));
  printq(qpow(phi_inv, 2));
  printq(output);

  q_sqrt5 fourteen = qadd(
			  qadd(qpow(phi, 5),
			       qpow(phi, 2)),
			  qadd(qpow(phi_inv, 3),
			       qpow(phi_inv, 6))
			  );
  printq(fourteen);
  printq(
	 qadd(qpow(phi, 5),
	      qpow(phi_inv, 6))
	 );
  printq(
	 qadd(qpow(phi, 2),
	      qpow(phi_inv, 3))
	 );

  int n;
  for (n = 0; n < 10; n++)
    {
      printf("phi^%d + phi^-%d = ", n, n+1);
      printq(
	     qadd(
		  qpow(phi, n),
		  qpow(phi_inv, n+1)
		  )
	     );
    }

  rational r = rational_construct(1, 0);
  printf("%d/%d\n", r.num, r.den);

}
