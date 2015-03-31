int count(int *xrows, int *xcols, int *x, int *d)
{
    int i, j, k, m;
    
    for (i=0; i < *xrows; i++)
	for (j=i+1; j<*xrows;j++)
	{
	    m = 0;
	    for (k=0; k < *xcols; k++)
		if (x[i+k*(*xrows)]!=x[j+k*(*xrows)])
		    m++;
	     d[m] = d[m]+1;
	}
    return 0;
}
