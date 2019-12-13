test_that("can parse an `init` attribute signature", {
  code <- to_lines("
    // [[ init() ]]
    void fn(DllInfo* dll) {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  x <- parse_signatures(attrs, code)

  expect <- list(list(name = "fn", loc = 3L))
  expect_equal(x$signature, expect)
})

test_that("can parse an `init` attribute signature with the `*` in a different spot", {
  code <- to_lines("
    // [[ init() ]]
    void fn(DllInfo *dll) {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  x <- parse_signatures(attrs, code)

  expect <- list(list(name = "fn", loc = 3L))
  expect_equal(x$signature, expect)

  code <- to_lines("
    // [[ init() ]]
    void fn(DllInfo * dll) {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  x <- parse_signatures(attrs, code)

  expect <- list(list(name = "fn", loc = 3L))
  expect_equal(x$signature, expect)
})

test_that("error with `DllInfo`", {
  code <- to_lines("
    // [[ init() ]]
    void fn(DllInfo dll) {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  expect_error(parse_signatures(attrs, code), "is a `DllInfo\\*`")
})

test_that("error with two arguments", {
  code <- to_lines("
    // [[ init() ]]
    void fn(DllInfo* dll, SEXP x) {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  expect_error(parse_signatures(attrs, code), "can only be 1 argument")
})

test_that("error with 0 arguments", {
  code <- to_lines("
    // [[ init() ]]
    void fn() {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  expect_error(parse_signatures(attrs, code), "can only be 1 argument")
})

test_that("error with non void return value", {
  code <- to_lines("
    // [[ init() ]]
    SEXP fn(DllInfo* dll) {
      return;
    }
  ")

  attrs <- parse_attributes(code)
  expect_error(parse_signatures(attrs, code), "return value of `void`")
})
