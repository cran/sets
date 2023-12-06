#include <R.h>
#include <Rdefines.h>

//#define __DEBUG

// Bit operations on compound variables.
static void _ior(int *x, int *y, int *r, int l)
{
    while (l-- > 0) 
	r[l] = x[l] | y[l];
}

static void _xor(int *x, int *y, int *r, int l)
{
    while (l-- > 0) 
	r[l] = x[l] ^ y[l];
}

static void _and(int *x, int *y, int *r, int l)
{
    while (l-- > 0) 
	r[l] = x[l] & y[l];
}

typedef void (*OPFUN)(int *, int *, int *, int);
static OPFUN ops[] = { _ior, _and, _xor };

// Compare compound variables.
static int _ieq(int *x, int *y, int l) {
    while (l-- > 0)
	if (x[l] != y[l])
	    return 0;
    return 1;
}

// Hash function for compound variables.
static int _hash(int *x, int l, int k) {
    unsigned int j = l * 100;

    k = 32 - k;
    while (l-- > 0) {
	j ^= 3141592653U * (unsigned int) x[l] >> k;
	j *= 97;
    }
   return 3141592653U * j >> k; 
}

// Add index to hash.
static int _hadd(SEXP x, int i, SEXP h, int k) {
    int j;
    SEXP s;

    s = VECTOR_ELT(x, i);
    k = _hash(INTEGER(s), LENGTH(s), k);
    while ((j = INTEGER(h)[k]) > -1) {
	if (_ieq(INTEGER(VECTOR_ELT(x, j)), INTEGER(s), LENGTH(s)))
	    return j;
	k = (k + 1) % LENGTH(h);
    }
    INTEGER(h)[k] = i;

    return -1;
}

// Compute the closure of a set of binary vectors
// under the basic binary operations (see above).
//
// NOTE xor should not be used for obvious reasons.
//
// Inputs are a logical matrix with the elements in
// the rows and the atoms in the columns, and second
// an integer vector specifying the operator to use.
//
// The worst time complexity is O(min(2^nc, nr^2)).
// Note that normally the runtime is order dependent,
// e.g. it takes less iterations if the atoms are
// in increasing order with respect to size (number
// of bits).
//
// Returns a logical matrix. The rows are in depth-
// first-search order.
//
// Version: 0.2
//
// (C) ceeboo 2008

SEXP sets_closure(SEXP x, SEXP R_op) {
    if (!x || !isMatrix(x) || TYPEOF(x) != LGLSXP)
	error("'x' not a logical matrix");
    if (!R_op || TYPEOF(R_op) != INTSXP)
	error("'op' not an integer vector");
    int i, j, k, l, n, nr, nc, nb, hk;
    int *p;
    OPFUN op;
    SEXP r, q, q0, s, t, t0, ht;

    nr = INTEGER(GET_DIM(x))[0];
    nc = INTEGER(GET_DIM(x))[1];
    if (!nc && nr)
	error("'x' invalid dimensions");
    if (nr < 2)
	return x;

    // FIXME does using all the bits also work
    //       on other platforms?
    nb = (int) ceil((double) nc / (sizeof(int) * CHAR_BIT));

    if (INTEGER(R_op)[0] < 1 ||
	INTEGER(R_op)[0] > sizeof(ops) / sizeof(OPFUN))
	error("'op' invalid value");

    op = ops[INTEGER(R_op)[0]-1]; 

    t = PROTECT(allocVector(VECSXP, nr));

    // Encode data. Note that the bits are
    // spread evenly across the variables.
    for (i = 0; i < nr; i++) {
	SET_VECTOR_ELT(t, i, (s = allocVector(INTSXP, nb)));
	memset(INTEGER(s), 0, sizeof(int) * nb);
	for (k = 0; k < nc; k++) {
	    j = k % nb;
	    INTEGER(s)[j] <<= 1;
	    INTEGER(s)[j]  |= LOGICAL(x)[i+k*nr];
	}
    }
    // Initialize hash table.
    if (nr > 1073741824)
	error("size %d too large for hashing", nr);
    k  = 2 * nr;
    n  = 2;
    hk = 1;
    while (k > n) {
	n  *= 2;
	hk += 1;
    }
    ht = PROTECT(allocVector(INTSXP, n));
    for (k = 0; k < n; k++)
	INTEGER(ht)[k] = -1;

    // Remove duplicates.
    n = 0;
    for (k = 0; k < nr; k++) {
	if (_hadd(t, k, ht, hk) > -1)
	    continue;
	if (n < k)
	    SET_VECTOR_ELT(t, n, VECTOR_ELT(t, k));
	n++;
    }
    nr = n;

    // Reset hash table.
    for (k = 0; k < LENGTH(ht); k++)
	INTEGER(ht)[k] = -1;

    // Result vector.
    n = 0;
    r = PROTECT(allocVector(VECSXP, nr));
    if (op == _xor) {
	SET_VECTOR_ELT(r, n, (s = allocVector(INTSXP, nb)));
	memset(INTEGER(s), 0, sizeof(int) * nb);
	_hadd(r, n++, ht, hk);
    }

    // Initialize stack.
    q = PROTECT(allocVector(VECSXP, nr + 1));
    for (k = 2; k < nr + 1; k++)
	SET_VECTOR_ELT(q, k, allocVector(INTSXP, nb));

    j = 1;
    p = INTEGER(PROTECT(allocVector(INTSXP, nr + 1)));
    p[1] = 0;

#ifdef __DEBUG
    i = 0;
    Rprintf("# %7s %7s %7s %7s\n", "iter", "size", "RLEN", "HLEN");
#endif
    while (j > 0) {
	t0 = VECTOR_ELT(t, p[j]); 
	if (j > 1)
	    op(INTEGER(t0), INTEGER(VECTOR_ELT(q, j-1)),
		      INTEGER((q0 = VECTOR_ELT(q, j))), nb);
	else
	    SET_VECTOR_ELT(q, j, (q0 = t0));

	// Resize hash table.
	if ((k = 2 * n) == LENGTH(ht)) {
	    if (n > 1073741824)
		error("size %d too large for hashing", n);
	    UNPROTECT_PTR(ht);

	    ht = PROTECT(allocVector(INTSXP, k * 2));
	    for (k = 0; k < LENGTH(ht); k++)
		INTEGER(ht)[k] = -1;
	    hk++;
	    for (k = 0; k < n; k++)
		_hadd(r, k, ht, hk);
	}
	// Resize result vector.
	if (n == LENGTH(r)) {
	    s = r;
	    r = PROTECT(allocVector(VECSXP, 2 * n));
	    for (k = 0; k < n; k++)
		SET_VECTOR_ELT(r, k, VECTOR_ELT(s, k));

	    UNPROTECT_PTR(s);
	}
	SET_VECTOR_ELT(r, n, q0);

	l = n;
	if (_hadd(r, n, ht, hk) == -1)
	    SET_VECTOR_ELT(r, n++, duplicate(q0));

	if (p[j] < nr - 1) {
	    if (n > l) {		// expand sequence
		j++;
		p[j] = p[j-1] + 1;
	    } else			// prune
		p[j]++;
	} else				// backtrack
	    p[--j]++;
#ifdef __DEBUG
	i++;
	if (j < 2)
	    Rprintf("# %7i %7i %7i %7i\n", i, n, LENGTH(r), LENGTH(ht));
#endif
	R_CheckUserInterrupt();
    }
    UNPROTECT(5);

    // Decode result.
    PROTECT(r);
    s = allocMatrix(LGLSXP, n, nc);
    for (i = 0; i < n; i++) {
	q = VECTOR_ELT(r, i);
	for (k = nc - 1; k > -1; k--) {
	    j = k % nb;
	    LOGICAL(s)[i+k*n] = INTEGER(q)[j] & 1;
	    INTEGER(q)[j] >>= 1;
	}
    }
    UNPROTECT(1);

    // Set colnames.
    if (!isNull((q = getAttrib(x, R_DimNamesSymbol)))) {
	PROTECT(s);
	setAttrib(s, R_DimNamesSymbol, (r = allocVector(VECSXP, 2)));
	SET_VECTOR_ELT(r, 0, R_NilValue);
	SET_VECTOR_ELT(r, 1, VECTOR_ELT(q, 1));

	UNPROTECT(1);
    }

    return s;
}

// From the R-2.7.1 source code.
#define NUMERIC int
void R_qsort_int_V(int *v, SEXP I, int i, int j)
#include "qsort-Vbody.h"
#undef NUMERIC

// Complement a compound variable.
static void _not(int *x, int *r, int l)
{
    while (l-- > 0)
	r[l] = ~x[l];
}

// Test if the first compound variable
// is a subset of the second.
static int _iss(int *x, int *y, int l)
{
    while (l-- > 0)
	if ((x[l] & y[l]) != x[l])
	    return 0;
    return 1;
}

// Compute the reduction (base) of a logical matrix
// under union or intersection, i.e. a unique minimal
// subset of the rows that spans the same space.
//
// cf. Doignon and Falmagne (1999). Knowledge Spaces.
//     Springer, pp. 29 -- 31.
//
// Inputs are as above.
//
// The worst time complexity is O(n^2). Note that
// r(x, &) == !r(!x, |), and vice versa.
//
// Returns a logical matrix.
//
// Version 0.1
//
// (C) ceeboo 2008

SEXP sets_reduction(SEXP x, SEXP R_op) {
    if (!x || !isMatrix(x) || TYPEOF(x) != LGLSXP)
	error("'x' not a logical matrix");
    if (!R_op || TYPEOF(R_op) != INTSXP)
	error("'op' not an integer vector");
    int i, j, k, l, n, nr, nc, nb;
    SEXP r, s, q, q0, t;

    nr = INTEGER(GET_DIM(x))[0];
    nc = INTEGER(GET_DIM(x))[1];
    if (!nc && nr)
	error("'x' invalid dimensions");
    if (nr < 2)
	return x;

    // FIXME does using all the bits also work
    //       on other platforms?
    nb = (int) ceil((double) nc / (sizeof(int) * CHAR_BIT));

    if (INTEGER(R_op)[0] != 1 &&
	INTEGER(R_op)[0] != 2)
	error("'op' invalid value");

    t = PROTECT(allocVector(VECSXP, nr));
    q = PROTECT(allocVector(INTSXP, nr));

    // Encode data.
    for (i = 0; i < nr; i++) {
	SET_VECTOR_ELT(t, i, (s = allocVector(INTSXP, nb)));
	memset(INTEGER(s), 0, sizeof(int) * nb);
	n = 0;
	for (k = 0; k < nc; k++) {
	    j = k % nb;
	    l = LOGICAL(x)[i+k*nr]; 
	    INTEGER(s)[j] <<= 1;
	    INTEGER(s)[j]  |= l;
	    n += l; 
	}
	// Transform to dual.
	if (INTEGER(R_op)[0] == 2) {
	    _not(INTEGER(s), INTEGER(s), nb);
	    INTEGER(q)[i] = nc - n;
	} else
	    INTEGER(q)[i] = n;
    }

    // Order breadth-first.
    R_qsort_int_V(INTEGER(q), t, 1, nr);
    UNPROTECT_PTR(q);

    // Remove duplicates.
    q = duplicated(t, FALSE);
    n = 0;
    for (i = 0; i < nr; i++) {
	if (LOGICAL(q)[i] == TRUE)
	    continue;
	if (n < i)
	    SET_VECTOR_ELT(t, n, VECTOR_ELT(t, i));
	n++;
    }
    nr = n;

    // Initialize.
    q = PROTECT(allocVector(INTSXP, nb));
    r = PROTECT(allocVector(VECSXP, nr));
    SET_VECTOR_ELT(r, 0, VECTOR_ELT(t, 0));
    n = 1;
#ifdef __DEBUG
    k = 0;
    Rprintf("# %7s %7s\n", "iter", "size");
#endif
    for (i = 1; i < nr; i++) {
	memset(INTEGER(q), 0, sizeof(int) * nb);
	s = VECTOR_ELT(t, i);
	for (j = i - 1; j > -1; j--) {
	    q0 = VECTOR_ELT(t, j);
	    if (!_iss(INTEGER(q0), INTEGER(s), nb))
		continue;
	    _ior(INTEGER(q0), INTEGER(q), INTEGER(q), nb);
	    if (_ieq(INTEGER(s), INTEGER(q), nb))
		goto next;
	}
	SET_VECTOR_ELT(r, n++, s);
    next:
#ifdef __DEBUG
	k += i - 1 - j;
	Rprintf("# %7i %7i\n", k, n);
#endif
	R_CheckUserInterrupt();
    }
    UNPROTECT_PTR(q);
    UNPROTECT_PTR(t);

    // Decode result.
    s = allocMatrix(LGLSXP, n, nc);
    for (i = 0; i < n; i++) {
	q = VECTOR_ELT(r, i);
	// Transform back.
	if (INTEGER(R_op)[0] == 2)
	    _not(INTEGER(q), INTEGER(q), nb);
	for (k = nc - 1; k > -1; k--) {
	    j = k % nb;
	    LOGICAL(s)[i+k*n] = INTEGER(q)[j] & 1;
	    INTEGER(q)[j] >>= 1;
	}
    }
    UNPROTECT(1);

    // Set colnames.
    if (!isNull((q = getAttrib(x, R_DimNamesSymbol)))) {
	PROTECT(s);
	setAttrib(s, R_DimNamesSymbol, (r = allocVector(VECSXP, 2)));
	// FIXME do we need rownames?
	SET_VECTOR_ELT(r, 0, R_NilValue);
	SET_VECTOR_ELT(r, 1, VECTOR_ELT(q, 1));

	UNPROTECT(1);
    }

    return s;
}

//
