# ------------------------------------------------------------------------------
# No attributes

test_that("correct behavior with no attributes", {
  code <- to_lines("
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, numeric())
  expect_equal(x$attributes, list())
})

# ------------------------------------------------------------------------------
# No parenthesis

test_that("can parse a single attribute with no parenthesis", {
  code <- to_lines("
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 2)

  expect <- list(list(export = new_argument_df()))
  expect_equal(x$attributes, expect)
})

test_that("can parse two attributes with no parenthesis", {
  code <- to_lines("
    // [[ export && register ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 2)

  expect <- list(list(
    export = new_argument_df(),
    register = new_argument_df()
  ))

  expect_equal(x$attributes, expect)
})

# ------------------------------------------------------------------------------
# Parenthesis

test_that("can parse an attribute with parenthesis", {
  code <- to_lines("
    // [[ export() ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 2)

  expect <- list(list(export = new_argument_df()))
  expect_equal(x$attributes, expect)
})

test_that("can parse an attribute with an argument", {
  code <- to_lines("
    // [[ export(name = fancy_fn) ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 2)

  expect <- new_argument_df("name", "fancy_fn")
  expect <- list(list(export = expect))

  expect_equal(x$attributes, expect)
})

test_that("can parse an attribute with two arguments", {
  code <- to_lines("
    // [[ export(name = fancy_fn, fancy = stuff) ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 2)

  expect <- new_argument_df(c("name", "fancy"), c("fancy_fn", "stuff"))
  expect <- list(list(export = expect))

  expect_equal(x$attributes, expect)
})

# ------------------------------------------------------------------------------
# Multiple functions

test_that("can parse multiple functions with multiple attributes", {
  code <- to_lines("
    // [[ export(x = y, z = w) && register() ]]
    SEXP fn(SEXP x) {
      return x;
    }

    // [[ export(foo = bar) ]]
    SEXP fn2(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, c(2, 7))

  args1 <- new_argument_df(c("x", "z"), c("y", "w"))
  args2 <- new_argument_df()
  args3 <- new_argument_df("foo", "bar")

  expect <- list(list(export = args1, register = args2), list(export = args3))

  expect_equal(x$attributes, expect)
})

# ------------------------------------------------------------------------------
# Spacing

test_that("can generally ignore non-standard spacing", {
  code <- to_lines("

    //[[export(x =y, z= w)&&   register()]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 3)

  args1 <- new_argument_df(c("x", "z"), c("y", "w"))
  args2 <- new_argument_df()

  expect <- list(list(export = args1, register = args2))

  expect_equal(x$attributes, expect)
})

