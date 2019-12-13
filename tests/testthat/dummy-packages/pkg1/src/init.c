//** File generated automatically by cbuild - please do not modify by hand

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <stdbool.h> // for bool
#include <R_ext/Rdynload.h>

// .Call declarations
extern SEXP test1_fn1(SEXP);
extern SEXP test1_fn2(SEXP);
extern SEXP test2_fn1(SEXP);
extern SEXP test2_fn2(SEXP);

// .Call entries
static const R_CallMethodDef CallEntries[] = {
  {"test1_fn1",          (DL_FUNC) &test1_fn1, 1},
  {"test1_fn2",          (DL_FUNC) &test1_fn2, 1},
  {"test2_fn1_override", (DL_FUNC) &test2_fn1, 1},
  {"test2_fn2",          (DL_FUNC) &test2_fn2, 1},
  {NULL, NULL, 0}
};

// .External declarations
extern SEXP test2_fn4(SEXP);

// .External2 declarations
extern SEXP test2_fn3(SEXP, SEXP, SEXP, SEXP);

// .External / .External2 entries
static const R_ExternalMethodDef ExtEntries[] = {
  {"test2_fn3", (DL_FUNC) &test2_fn3, 2},
  {"test2_fn4", (DL_FUNC) &test2_fn4, 2},
  {NULL, NULL, 0}
};

// Callable API declarations
extern SEXP test1_fn3(SEXP);

// Hidden callable API declarations
extern SEXP test1_fn4(SEXP);

// Init hook declarations
void test1_fn5(DllInfo* dll);
void test1_fn6(DllInfo* dll);

void R_init_test(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
  R_useDynamicSymbols(dll, FALSE);

  // Hidden callable API registrations
  R_RegisterCCallable("test", "test1_fn4", (DL_FUNC) &test1_fn4);

  // Callable API registrations
  R_RegisterCCallable("test", "test1_fn2",          (DL_FUNC) &test1_fn2);
  R_RegisterCCallable("test", "test1_fn3",          (DL_FUNC) &test1_fn3);
  R_RegisterCCallable("test", "test2_fn2_callable", (DL_FUNC) &test2_fn2);

  test1_fn5(dll);
  test1_fn6(dll);
}

