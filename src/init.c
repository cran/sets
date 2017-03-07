#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP sets_closure(SEXP x, SEXP R_op);
SEXP sets_reduction(SEXP x, SEXP R_op);

static const R_CallMethodDef CallEntries[] = {
    {"sets_closure", (DL_FUNC) &sets_closure, 2},
    {"sets_reduction", (DL_FUNC) &sets_reduction, 2},
    {NULL, NULL, 0}
};

void R_init_sets(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
