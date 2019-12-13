
test_that("correct behavior with no attributes", {
  code <- to_lines("
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- parse_attributes(code)

  expect_equal(x, new_attribute_df())
})

test_that("ignored if no opening brackets", {
  code <- "// export() ]]"

  x <- parse_attributes(code)

  expect_equal(x, new_attribute_df())
})

test_that("ignored if no closing brackets", {
  code <- "// [[ export()"

  x <- parse_attributes(code)

  expect_equal(x, new_attribute_df())
})

test_that("can parse a single attribute", {
  code <- "// [[ export() ]]"

  x <- parse_attributes(code)

  expect_equal(x$loc, 1)
  expect_equal(x$attribute, "export")

  args <- list(list(name = NA_character_))
  expect_equal(x$args, args)
})

test_that("can parse an attribute with an argument", {
  code <- "// [[ export(name = 'fancy_fn') ]]"

  x <- parse_attributes(code)

  args <- list(list(name = "fancy_fn"))
  expect_equal(x$args, args)
})

test_that("can parse an attribute with two arguments", {
  code <- "// [[ export_external(name = 'fancy_fn', n = 1) ]]"

  x <- parse_attributes(code)

  args <- list(list(name = "fancy_fn", n = 1L))
  expect_equal(x$args, args)
})

test_that("can parse two attributes", {
  code <- "// [[ export(); callable() ]]"

  x <- parse_attributes(code)

  expect_equal(x$loc, c(1, 1))
  expect_equal(x$attribute, c("export", "callable"))

  args <- list(
    list(name = NA_character_),
    list(name = NA_character_, hidden = FALSE)
  )

  expect_equal(x$args, args)
})

test_that("can parse multiple functions with multiple attributes", {
  code <- to_lines("
    // [[ export(name = 'nm'); callable() ]]
    SEXP fn(SEXP x) {
      return x;
    }

    // [[ export(name = 'nm2') ]]
    SEXP fn2(SEXP x) {
      return x;
    }
  ")

  x <- parse_attributes(code)

  expect_equal(x$loc, c(2, 2, 7))
  expect_equal(x$attribute, c("export", "callable", "export"))

  args <- list(
    list(name = "nm"),
    list(name = NA_character_, hidden = FALSE),
    list(name = "nm2")
  )

  expect_equal(x$args, args)
})

test_that("can parse an `init` attribute", {
  code <- "// [[ init() ]]"

  x <- parse_attributes(code)

  expect <- new_attribute_df(loc = 1, attribute = "init", args = list(list()))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# Spacing

test_that("can generally ignore non-standard spacing", {
  code <- to_lines("

    //[[export_external(n = 2,name ='fn2');   callable()]]
    SEXP fn(SEXP x) {
      return x;
    }
  ")

  x <- parse_attributes(code)

  expect_equal(x$loc, c(3, 3))
  expect_equal(x$attribute, c("export_external", "callable"))

  args <- list(
    list(name = "fn2", n = 2L),
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
    parse_attributes(code),
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
    parse_attributes(code),
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
    parse_attributes(code),
    'unused argument [(]names = "fn"[)]'
  )
})





