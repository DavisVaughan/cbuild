test_that("can knit a cbuild chunk", {
  dir <- tempdir()
  path_knitr <- file.path(dir, "knitr")
  file.create(path_knitr)

  on.exit(unlink(path_knitr, recursive = TRUE, force = TRUE), add = TRUE)

  knitr::knit(
    test_path("knitr/test-1.Rmd"),
    path_knitr,
    quiet = TRUE
  )

  verify_output(
    test_path("output/knitr-test-1.txt"),
    cat(readLines(path_knitr), sep = "\n")
  )
})

test_that("can change the environment", {
  dir <- tempdir()
  path_knitr <- file.path(dir, "knitr")
  file.create(path_knitr)

  on.exit(unlink(path_knitr, recursive = TRUE, force = TRUE), add = TRUE)

  knitr::knit(
    test_path("knitr/test-2.Rmd"),
    path_knitr,
    quiet = TRUE
  )

  verify_output(
    test_path("output/knitr-test-2.txt"),
    cat(readLines(path_knitr), sep = "\n")
  )
})

test_that("can add includes", {
  dir <- tempdir()
  path_knitr <- file.path(dir, "knitr")
  file.create(path_knitr)

  on.exit(unlink(path_knitr, recursive = TRUE, force = TRUE), add = TRUE)

  knitr::knit(
    test_path("knitr/test-3.Rmd"),
    path_knitr,
    quiet = TRUE
  )

  verify_output(
    test_path("output/knitr-test-3.txt"),
    cat(readLines(path_knitr), sep = "\n")
  )
})

test_that("can turn off no_remap", {
  dir <- tempdir()
  path_knitr <- file.path(dir, "knitr")
  file.create(path_knitr)

  on.exit(unlink(path_knitr, recursive = TRUE, force = TRUE), add = TRUE)

  knitr::knit(
    test_path("knitr/test-4.Rmd"),
    path_knitr,
    quiet = TRUE
  )

  verify_output(
    test_path("output/knitr-test-4.txt"),
    cat(readLines(path_knitr), sep = "\n")
  )
})
