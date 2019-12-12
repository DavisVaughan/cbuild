#' Source a single C function
#'
#' `source_function()` is the most basic way to source a C function to the R
#' side. It sources a single C function and exports it on the R side. There
#' is no need to include the `// [[ export() ]]` tag when
#' using `source_function()`, since only one function will be exported.
#'
#' @inheritParams source_file
#'
#' @param x `[character(1)]`
#'
#'   A block of code containing a single C function to compile.
#'
#' @return
#' An R function that calls the compiled C code.
#'
#' @examples
#' code <- "
#'   SEXP fn(SEXP x) {
#'     return x;
#'   }
#' "
#'
#' fn <- source_function(code)
#'
#' fn(1)
#' @export
source_function <- function(x, includes = NULL, no_remap = TRUE, show = FALSE) {
  x <- remove_leading_blank_lines(x)
  x <- add_export_attribute(x)

  out <- source_code(x, includes = includes, no_remap = no_remap, show = show)

  out[[1]]
}

add_export_attribute <- function(x) {
  c(
    "// [[ export() ]]",
    x
  )
}

# To prevent space between the function and the export attribute
remove_leading_blank_lines <- function(x) {
  while(grepl("^\n", x)) {
    x <- gsub("^\n", "", x)
  }
  x
}
