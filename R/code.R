#' @export
source_code <- function(code, includes = NULL, remap = FALSE, show = FALSE) {
  temp_file <- tempfile(fileext = ".c")
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  write_lines(temp_file, code)

  source_file(temp_file, includes = includes, remap = remap, show = show)
}
