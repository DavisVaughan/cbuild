#' Source a block of text containing C code
#'
#' `source_code()` will parse through `x` looking for functions tagged with
#' `// [[ export() ]]` and will compile the code block and export those
#' functions to the R side.
#'
#' @inheritParams source_file
#'
#' @param x `[character(1)]`
#'
#'   A block of C code to compile.
#'
#' @return
#' A named list containing the functions specified for export.
#'
#' @examples
#' code <- "
#'   static SEXP helper(SEXP x) {
#'     return x;
#'   }
#'
#'   // [[ export() ]]
#'   SEXP fn1(SEXP x) {
#'     return helper(x);
#'   }
#'
#'   // [[ export() ]]
#'   SEXP fn2(SEXP x, SEXP y) {
#'     double result = REAL(x)[0] + REAL(y)[0];
#'     return Rf_ScalarReal(result);
#'   }
#' "
#'
#' sourced <- source_code(code)
#'
#' sourced$fn1(1)
#' sourced$fn2(1, 2)
#' @export
source_code <- function(x, includes = NULL, no_remap = TRUE, show = FALSE) {
  temp_file <- tempfile(fileext = ".c")
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  write_lines(temp_file, x, sep = "\n")

  source_file(temp_file, includes = includes, no_remap = no_remap, show = show)
}
