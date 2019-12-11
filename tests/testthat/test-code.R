test_that("can source a code block", {
  code <- "
    // [[ cbuild::export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  "

  x <- source_code(code)
  expect_equal(x$fn(1), 1)
})

test_that("can source two functions", {
  code <- "
    // [[ cbuild::export ]]
    SEXP fn(SEXP x) {
      return x;
    }

    // [[ cbuild::export ]]
    SEXP fn2(SEXP x) {
      return x;
    }
  "

  x <- source_code(code)
  expect_equal(x$fn(1), 1)
  expect_equal(x$fn2(1), 1)
})

test_that("can source a code block that uses a helper", {
  code <- "
    static SEXP helper(SEXP x) {
      return x;
    }

    // [[ cbuild::export ]]
    SEXP fn(SEXP x) {
      return helper(x);
    }
  "

  x <- source_code(code)
  expect_equal(x$fn(1), 1)
  expect_equal(x$helper, NULL)
})

test_that("must have an attribute tag", {
  code <- "
    SEXP fn(SEXP x) {
      return x;
    }
  "

  expect_error(source_code(code), "At least 1 function")
})

test_that("can source with remap", {
  code <- "
    // [[ cbuild::export ]]
    SEXP fn(SEXP x) {
      return ScalarInteger(1);
    }
  "

  x <- source_code(code, remap = TRUE)

  expect_equal(x$fn(1), 1)
})
