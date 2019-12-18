// [[ export() ]]
SEXP test1_fn1(SEXP x) {
  return x;
}

// [[ export(); callable() ]]
SEXP test1_fn2(SEXP y) {
  return y;
}

// [[ callable() ]]
SEXP test1_fn3(SEXP y) {
  return y;
}

// [[ callable(hidden = TRUE) ]]
SEXP test1_fn4(SEXP y) {
  return y;
}

// [[ init() ]]
void test1_fn5(DllInfo* dll) {
  return;
}

// [[ init() ]]
void test1_fn6(DllInfo* dll) {
  return;
}

// [[ callable() ]]
R_len_t test1_fn7(R_len_t y, int *x) {
  return y;
}
