test_that("can source a function", {
  code <- "SEXP fn(SEXP x) {
             return x;
           }"

  fn <- source_function(code)
  expect_equal(fn(1), 1)
})

test_that("can source a function with blank lines above it", {
  code <- "

    SEXP fn(SEXP x) {
      return x;
    }

  "

  fn <- source_function(code)
  expect_equal(fn(1), 1)
})
