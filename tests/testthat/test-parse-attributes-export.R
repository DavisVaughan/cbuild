test_that("can parse signatures over two lines", {
  code <- to_lines("
    // [[ export() ]]
    SEXP fn(SEXP x,
            SEXP y) {
      return x;
    }
  ")

  x <- parse_exports(code)
  expect_equal(x[[1]]$args, c("x", "y"))
})

test_that("can parse signatures over three lines", {
  code <- to_lines("
    // [[ export() ]]
    SEXP fn(SEXP x,
            SEXP y,
            SEXP z) {
      return x;
    }
  ")

  x <- parse_exports(code)
  expect_equal(x[[1]]$args, c("x", "y", "z"))
})

test_that("can parse signatures when the closing parenthesis is on its own line", {
  code <- to_lines("
    // [[ export() ]]
    SEXP fn(SEXP x,
            SEXP y,
            SEXP z
            ) {
      return x;
    }
  ")

  x <- parse_exports(code)
  expect_equal(x[[1]]$args, c("x", "y", "z"))
})
