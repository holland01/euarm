#include <stdio.h>
#include <math.h>
#include <string.h>

#define neg_sign(s) (((s) & 0x1) ^ (((s) & 0x2) >> 1))
#define abs(x) (((x) < 0) ? -(x): (x))

int simple_mul(int a, int b)
{
	char sign = 0;
	int i, u, v, q;

	if (a < 0) sign |= 0x1;
	if (b < 0) sign |= 0x2;

	if (a > b) {
		u = abs(a);
		v = abs(b);
	} else {
		u = abs(b);
		v = abs(a);
	}

	for (i = 0, q = 0; i < v; ++i) {
		q += abs(u);
	}

	if (neg_sign(sign))
		q = -q;

	return q;
}

int mul(int a, int b)
{
	int rem, x;
	unsigned char sign, xj;
	int sums[9];

	x = abs(b);
	rem = 0;
	xj = 0;
	sign = 0;

	if (a < 0) sign |= 0x1;
	if (b < 0) sign |= 0x2;

	memset(sums, 0, sizeof(sums));

	while (x > 0) {
		char dx, yj;
		int y;
		y = abs(a);
		dx = x % 10;
		yj = 0;
		while (y > 0) {
			char dy, u;
			dy = y % 10;
			u = simple_mul(dx, dy) + rem;
			if (u >= 10) {
				rem = u / 10;
			} else {
				rem = 0;
			}
			sums[xj] += simple_mul((u % 10), pow(10, yj));
			y = y / 10;
			yj++;
		}
		x = x / 10;
		xj++;
	}

	if (rem != 0) {
		sums[xj++] = rem;
	}

	{
		int u, v;
		for (v = 0, u = 0; v < xj; ++v) {
			u += simple_mul(sums[v], pow(10, v));
		}

		if (neg_sign(sign))
			u = -u;

		return u;
	}
}

int main(void)
{
	int a, b, c;
	a = -233;
	b = -3333;
	c = mul(a, b);
	printf("%i * %i = %i\n", a, b, c);
	return 0;
}

