// [[ export(name = "test2_fn1_override") ]]
SEXP test2_fn1(SEXP x) {
  return x;
}


// [[ export(); callable(name = "test2_fn2_callable") ]]
SEXP test2_fn2(SEXP y) {
  return y;
}

// [[ export_external2(n = 2) ]]
SEXP test2_fn3(SEXP call, SEXP op, SEXP args, SEXP env) {
  return y;
}

// [[ export_external(n = 2) ]]
SEXP test2_fn4(SEXP args) {
  return y;
}
