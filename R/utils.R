# xfun::read_utf8(), also used by reprex
read_lines <- function(con, error = TRUE) {
  opts = options(encoding = "native.enc")
  on.exit(options(opts), add = TRUE)
  x = readLines(con, encoding = "UTF-8", warn = FALSE)
  i = which(!is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8")))
  n = length(i)
  if (n > 0)
    (if (error)
      stop
     else warning)(if (is.character(con))
       c("The file ", con, " is not encoded in UTF-8. "),
       "These lines contain invalid UTF-8 characters: ",
       paste(c(head(i), if (n > 6) "..."), collapse = ", "))
  x
}

write_lines <- function(file, lines) {
  writeLines(lines, file)
  invisible(file)
}

new_attribute_df <- function(loc = integer(), attributes = list()) {
  data_frame(loc = loc, attributes = attributes)
}

new_argument_df <- function(name = character(), value = character()) {
  data_frame(name = name, value = value)
}

data_frame <- function(...) {
  x <- list(...)

  size <- length(x) == 0L

  if (size) {
    n <- 0L
  } else {
    n <- length(x[[1L]])
  }

  if (size) {
    names(x) <- character()
  }

  new_attributes <- list(
    names = names(x),
    class = "data.frame",
    row.names = .set_row_names(n)
  )

  attributes(x) <- new_attributes

  x
}
