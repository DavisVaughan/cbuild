#' @export
source_code <- function(code, includes = NULL, no_remap = TRUE, show = FALSE) {
  temp_file <- tempfile(fileext = ".c")
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  write_lines(temp_file, code)

  source_file(temp_file, includes = includes, no_remap = no_remap, show = show)
}
