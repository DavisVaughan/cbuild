// [[ export(name = "test2_fn1_override") ]]
SEXP test2_fn1(SEXP x) {
  return x;
}


// [[ export(); callable(name = "test2_fn2_callable") ]]
SEXP test2_fn2(SEXP y) {
  return y;
}
