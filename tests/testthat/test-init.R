# ------------------------------------------------------------------------------
# `path` validation

test_that("`path` must point to an existing directory", {
  expect_error(write_init("hi"), "existing folder containing an R package")
})

test_that("`path` must point to a directory with a src folder", {
  dir <- tempdir()
  expect_error(write_init(dir), "with a `src` folder")
})

test_that("must have a DESCRIPTION file", {
  dir <- tempdir()
  dir_pkg <- file.path(dir, "pkg")
  dir_src <- file.path(dir_pkg, "src")

  dir.create(dir_pkg)
  dir.create(dir_src)

  on.exit(unlink(dir_pkg, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(dir_src, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(write_init(dir_pkg), "A `DESCRIPTION` file")
})

test_that("can overwrite if no init.c file", {
  dir <- tempdir()
  dir_pkg <- file.path(dir, "pkg")
  dir_src <- file.path(dir_pkg, "src")

  dir.create(dir_pkg)
  dir.create(dir_src)

  on.exit(unlink(dir_pkg, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(dir_src, recursive = TRUE, force = TRUE), add = TRUE)

  expect_true(can_write_init(dir_pkg))
})

test_that("can overwrite if init.c was created by cbuild", {
  dir <- tempdir()
  dir_pkg <- file.path(dir, "pkg")
  dir_src <- file.path(dir_pkg, "src")

  dir.create(dir_pkg)
  dir.create(dir_src)

  on.exit(unlink(dir_pkg, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(dir_src, recursive = TRUE, force = TRUE), add = TRUE)

  path_init <- path_init(dir_pkg)
  lines <- write_do_not_modify(character())
  write_lines(path_init, lines)

  expect_true(can_write_init(dir_pkg))
})

# ------------------------------------------------------------------------------
# Input validation

test_that("`path` is validated", {
  expect_error(write_init(path = 1), "must be a string")
  expect_error(write_init(path = c("x", "y")), "must be a string")
  expect_error(write_init(path = NA_character_), "must be a string")
})

test_that("`debug` is validated", {
  expect_error(write_init(path = 1), "must be a bool")
  expect_error(write_init(debug = c(TRUE, FALSE)), "must be a bool")
  expect_error(write_init(debug = NA), "must be a bool")
})
