to_lines <- function(x) {
  tf <- tempfile(fileext = ".c")
  writeLines(x, tf)
  readLines(tf)
}
