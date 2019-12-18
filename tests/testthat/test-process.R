# ------------------------------------------------------------------------------
# init.c generation

test_that("`src` directory with no `.c` files", {
  x <- process_attributes(path_package("test-1"), debug = TRUE)

  verify_output(
    test_path("output/test-1.txt"),
    cat2(x$init)
  )
})

test_that("`src` directory with just a `init.c` file that we created", {
  x <- process_attributes(path_package("test-2"), debug = TRUE)

  verify_output(
    test_path("output/test-2.txt"),
    cat2(x$init)
  )
})

test_that("`src` directory with .c files, but none are exported", {
  x <- process_attributes(path_package("test-3"), debug = TRUE)

  verify_output(
    test_path("output/test-3.txt"),
    cat2(x$init)
  )
})

test_that("can write an init file for a single function", {
  x <- process_attributes(path_package("test-4"), debug = TRUE)

  verify_output(
    test_path("output/test-4.txt"),
    cat2(x$init)
  )
})

test_that("can write an init file for two functions", {
  x <- process_attributes(path_package("test-5"), debug = TRUE)

  verify_output(
    test_path("output/test-5.txt"),
    cat2(x$init)
  )
})

test_that("can write an init file for two functions in different files", {
  x <- process_attributes(path_package("test-6"), debug = TRUE)

  verify_output(
    test_path("output/test-6.txt"),
    cat2(x$init)
  )
})

# ------------------------------------------------------------------------------
# callable api generation

test_that("can generate callable api files", {
  x <- process_attributes(path_package("api-test-1"), debug = TRUE)

  verify_output(
    test_path("output/api-test-1.txt"),
    {
      cat2(x$init)
      cat2(x$api_c)
      cat2(x$api_h)
    }
  )
})

# ------------------------------------------------------------------------------
# `path` validation

test_that("`path` must point to an existing directory", {
  expect_error(process_attributes("hi"), "existing folder containing an R package")
})

test_that("`path` must point to a directory with a src folder", {
  dir <- tempdir()
  expect_error(process_attributes(dir), "with a `src` folder")
})

test_that("must have a DESCRIPTION file", {
  dir <- tempdir()
  dir_pkg <- file.path(dir, "pkg")
  dir_src <- file.path(dir_pkg, "src")

  dir.create(dir_pkg)
  dir.create(dir_src)

  on.exit(unlink(dir_pkg, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(dir_src, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(process_attributes(dir_pkg), "A `DESCRIPTION` file")
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
  expect_error(process_attributes(path = 1), "must be a string")
  expect_error(process_attributes(path = c("x", "y")), "must be a string")
  expect_error(process_attributes(path = NA_character_), "must be a string")
})

test_that("`debug` is validated", {
  expect_error(process_attributes(debug = 1), "must be a bool")
  expect_error(process_attributes(debug = c(TRUE, FALSE)), "must be a bool")
  expect_error(process_attributes(debug = NA), "must be a bool")
})
