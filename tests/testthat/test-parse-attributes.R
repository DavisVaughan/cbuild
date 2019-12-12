
test_that("correct behavior with no attributes", {
  code <- to_lines("
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x, new_attribute_df())
})

test_that("ignored if no opening brackets", {
  code <- to_lines("
    // export() ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x, new_attribute_df())
})

test_that("ignored if no closing brackets", {
  code <- to_lines("
    // [[ export()
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x, new_attribute_df())
})

test_that("can parse a single attribute", {
  code <- to_lines("
    // [[ export() ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, 2)
  expect_equal(x$type, "export")

  args <- list(list(name = NA_character_, type = "call"))
  expect_equal(x$args, args)
})

test_that("can parse an attribute with an argument", {
  code <- to_lines("
    // [[ export(name = 'fancy_fn') ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  args <- list(list(name = "fancy_fn", type = "call"))
  expect_equal(x$args, args)
})

test_that("can parse an attribute with two arguments", {
  code <- to_lines("
    // [[ export(name = 'fancy_fn', type = 'external') ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  args <- list(list(name = "fancy_fn", type = "external"))
  expect_equal(x$args, args)
})

test_that("can parse two attributes", {
  code <- to_lines("
    // [[ export(); callable() ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, c(2, 2))
  expect_equal(x$type, c("export", "callable"))

  args <- list(
    list(name = NA_character_, type = "call"),
    list(name = NA_character_, hidden = FALSE)
  )

  expect_equal(x$args, args)
})

test_that("can parse multiple functions with multiple attributes", {
  code <- to_lines("
    // [[ export(name = 'nm', type = 'call'); callable() ]]
    SEXP fn(SEXP x) {
      return x;
    }

    // [[ export(name = 'nm2') ]]
    SEXP fn2(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, c(2, 2, 7))
  expect_equal(x$type, c("export", "callable", "export"))

  args <- list(
    list(name = "nm", type = "call"),
    list(name = NA_character_, hidden = FALSE),
    list(name = "nm2", type = "call")
  )

  expect_equal(x$args, args)
})

test_that("can parse an `init` attribute", {
  code <- to_lines("
    // [[ init() ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect <- new_attribute_df(loc = 2, type = "init", args = list(list()))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# Spacing

test_that("can generally ignore non-standard spacing", {
  code <- to_lines("

    //[[export(name ='fn2', type= 'external');   callable()]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- locate_and_parse_attributes(code)

  expect_equal(x$loc, c(3, 3))
  expect_equal(x$type, c("export", "callable"))

  args <- list(
    list(name = "fn2", type = "external"),
    list(name = NA_character_, hidden = FALSE)
  )

  expect_equal(x$args, args)
})

# ------------------------------------------------------------------------------
# Errors

test_that("error if valid function, but not called with parenthesis", {
  code <- to_lines("
    // [[ export ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  expect_error(
    locate_and_parse_attributes(code),
    "like `export[(][)]`, not `export`"
  )
})

test_that("error if invalid function", {
  code <- to_lines("
    // [[ stuff() ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  expect_error(
    locate_and_parse_attributes(code),
    'could not find function "stuff"'
  )
})

test_that("error if invalid function arguments", {
  code <- to_lines("
    // [[ export(names = 'fn') ]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  expect_error(
    locate_and_parse_attributes(code),
    'unused argument [(]names = "fn"[)]'
  )
})





