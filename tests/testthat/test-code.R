test_that("can source a code block", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  "

  x <- source_code(code)
  expect_equal(x$fn(1), 1)
})

test_that("can source two functions", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }

    // [[ export ]]
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

    // [[ export ]]
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

# ------------------------------------------------------------------------------
# `no_remap`

test_that("can source without remapping", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return ScalarInteger(1);
    }
  "

  x <- source_code(code, no_remap = FALSE)

  expect_equal(x$fn(1), 1)
})

# ------------------------------------------------------------------------------
# `includes`

test_that("can provide includes manually", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return AS_LOGICAL(x);
    }
  "

  x <- source_code(code, includes = "Rdefines.h")

  expect_equal(x$fn(1), TRUE)
})

test_that("must provide at least one include", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  "

  expect_error(source_code(code, includes = character()), "At least one `includes`")
})

test_that("don't use angled brackets in includes", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  "

  expect_error(source_code(code, includes = "<R.h>"), "should not contain angled brackets")
})

test_that("don't use `#include` in includes", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  "

  expect_error(source_code(code, includes = "#include <R.h>"), "should not contain `#include`")
})

test_that("includes must be a character", {
  code <- "
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  "

  expect_error(source_code(code, includes = 1), "must be a character vector")
})
