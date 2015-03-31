/* median.f -- translated by f2c (version 19960717).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include <R.h>

/* Subroutine */ double CC_median(x, n)
double *x;
Sint *n;
{
  double xmed;
  extern /* Subroutine */ int CC_sort();
    static Sint n2;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    CC_sort(n, &x[1]);
    n2 = *n / 2;
    if (n2 << 1 == *n) {
	xmed = (x[n2] + x[n2 + 1]) * (float).5;
    } else {
	xmed = x[n2 + 1];
    }
    return xmed;
} /* CC_median */

/* Subroutine */ int CC_sort(n, ra)
Sint *n;
double *ra;
{

    static Sint i__, j, l, ir;
    static double rra;
    /* Parameter adjustments */
    --ra;
    /* Function Body */
    l = *n / 2 + 1;
    ir = *n;
L10:

    if (l > 1) {
	--l;
	rra = ra[l];
    } else {
	rra = ra[ir];
	ra[ir] = ra[1];
	--ir;
	if (ir == 1) {
	    ra[1] = rra;
	    return 0;
	}
    }
    i__ = l;
    j = l + l;
L20:
    if (j <= ir) {
	if (j < ir) {
	    if (ra[j] < ra[j + 1]) {
		++j;
	    }
	}
	if (rra < ra[j]) {
	    ra[i__] = ra[j];
	    i__ = j;
	    j += j;
	} else {
	    j = ir + 1;
	}
	goto L20;
    }
    ra[i__] = rra;
    goto L10;
} /* CC_sort */


