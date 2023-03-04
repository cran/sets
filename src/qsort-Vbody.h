/*====== BODY of R_qsort_int_V() function ==============================
 *
 * is included with NUMERIC defined
 *======================================================================
*/
{
/* Orders v[] increasingly. Puts into I[] the permutation vector:
 *  new v[k] = old v[I[k]]
 * Only elements [i : j]  (in 1-indexing !)  are considered.

 * This is a modification of CACM algorithm #347 by R. C. Singleton,
 * which is a modified Hoare quicksort.
 * This version incorporates the modification in the remark by Peto.
*/
// retained for future customizing
#define qsort_Index
    if (TYPEOF(I) != VECSXP)
	error("'I' not of type list");
    int il[31], iu[31];
    /* Arrays iu[k] and il[k] permit sorting up to 2^(k+1)-1 elements;
     * originally k = 20 -> n_max =    2'097'151
     * now        k = 31 -> n_max = 4294'967'295
     */
    NUMERIC vt, vtt;
    double R = 0.375;
    int ii, ij, k, l, m;
#ifdef qsort_Index
    SEXP it, tt;
#endif


    /* 1-indexing for I[], v[]  (and `i' and `j') : */
    --v;

    ii = i;/* save */
    m = 1;

  L10:
    if (i < j) {
	if (R < 0.5898437) R += 0.0390625; else R -= 0.21875;
      L20:
	k = i;
	/* ij = (j + i) >> 1; midpoint */
	ij = i + (int)((j - i)*R);
#ifdef qsort_Index
	it = VECTOR_ELT(I, ij-1);
#endif
	vt = v[ij];
	if (v[i] > vt) {
#ifdef qsort_Index
	    SET_VECTOR_ELT(I, ij-1, VECTOR_ELT(I, i-1));
	    SET_VECTOR_ELT(I, i-1, it);
	    it = VECTOR_ELT(I, ij-1);
#endif
	    v[ij] = v[i]; v[i] = vt; vt = v[ij];
	}
	/* L30:*/
	l = j;
	if (v[j] < vt) {
#ifdef qsort_Index
	    SET_VECTOR_ELT(I, ij-1, VECTOR_ELT(I, j-1));
	    SET_VECTOR_ELT(I, j-1, it);
	    it = VECTOR_ELT(I, ij-1);
#endif
	    v[ij] = v[j]; v[j] = vt; vt = v[ij];
	    if (v[i] > vt) {
#ifdef qsort_Index
		SET_VECTOR_ELT(I, ij-1, VECTOR_ELT(I, i-1));
		SET_VECTOR_ELT(I, i-1, it);
		it = VECTOR_ELT(I, ij-1);
#endif
		v[ij] = v[i]; v[i] = vt; vt = v[ij];
	    }
	}

	for(;;) { /*L50:*/
	    do l--;  while (v[l] > vt);

#ifdef qsort_Index
	     tt = VECTOR_ELT(I, l-1);
#endif
	    vtt = v[l];
	    /*L60:*/ do k++;  while (v[k] < vt);

	    if (k > l) break;

	    /* else (k <= l) : */
#ifdef qsort_Index
	    SET_VECTOR_ELT(I, l-1, VECTOR_ELT(I, k-1));
	    SET_VECTOR_ELT(I, k-1, tt);
#endif
	    v[l] = v[k]; v[k] = vtt;
	}

	m++;
	if (l - i <= j - k) {
	    /*L70: */
	    il[m] = k;
	    iu[m] = j;
	    j = l;
	}
	else {
	    il[m] = i;
	    iu[m] = l;
	    i = k;
	}
    }
    else { /* i >= j : */

    L80:
	if (m == 1)	return;

	/* else */
	i = il[m];
	j = iu[m];
	m--;
    }

    if (j - i > 10)	goto L20;

    if (i == ii)	goto L10;

    --i;
  L100:
    do {
	++i;
	if (i == j) {
	    goto L80;
	}
#ifdef qsort_Index
	it = VECTOR_ELT(I, i);
#endif
	vt = v[i + 1];
    } while (v[i] <= vt);

    k = i;

    do { /*L110:*/
#ifdef qsort_Index
	SET_VECTOR_ELT(I, k, VECTOR_ELT(I, k-1));
#endif
	v[k + 1] = v[k];
	--k;
    } while (vt < v[k]);

#ifdef qsort_Index
    SET_VECTOR_ELT(I, k, it);
#endif
    v[k + 1] = vt;
    goto L100;
#undef qsort_Index
}
