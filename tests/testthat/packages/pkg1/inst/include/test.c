// File generated automatically by cbuild - please do not modify by hand

#include "test.h"

SEXP (*test1_fn2)(SEXP) = NULL;
SEXP (*test1_fn3)(SEXP) = NULL;
R_len_t (*test1_fn7)(R_len_t, int *) = NULL;
SEXP (*test2_fn2_callable)(SEXP) = NULL;

void test_init_api() {
  test1_fn2 = (SEXP (*)(SEXP)) R_GetCCallable("test", "test1_fn2");
  test1_fn3 = (SEXP (*)(SEXP)) R_GetCCallable("test", "test1_fn3");
  test1_fn7 = (R_len_t (*)(R_len_t, int *)) R_GetCCallable("test", "test1_fn7");
  test2_fn2_callable = (SEXP (*)(SEXP)) R_GetCCallable("test", "test2_fn2_callable");
}
