#' @export
source_function <- function(code, includes = NULL, remap = FALSE, show = FALSE) {
  code <- remove_leading_blank_lines(code)
  code <- add_export_attribute(code)

  out <- source_code(code, includes = includes, remap = remap, show = show)

  out[[1]]
}

add_export_attribute <- function(code) {
  c(
    "// [[ export ]]",
    code
  )
}

# To prevent space between the function and the export attribute
remove_leading_blank_lines <- function(code) {
  while(grepl("^\n", code)) {
    code <- gsub("^\n", "", code)
  }
  code
}
