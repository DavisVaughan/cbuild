hook_export <- function(..., name = NA_character_) {
  validate_no_dots(...)
  validate_name(name)

  args <- list(
    name = name
  )

  new_argument_df(attribute = "export", args = list(args))
}

hook_export_external2 <- function(..., n, name = NA_character_) {
  validate_no_dots(...)
  validate_name(name)

  if (missing(n)) {
    abort(
      ".External2 functions require the `n` argument. ",
      "Like: `// [[ export_external2(n = _) ]]`."
    )
  }

  n <- validate_n(n)

  args <- list(
    name = name,
    n = n
  )

  new_argument_df(attribute = "export_external2", args = list(args))
}

hook_export_external <- function(..., n, name = NA_character_) {
  validate_no_dots(...)
  validate_name(name)

  if (missing(n)) {
    abort(
      ".External functions require the `n` argument. ",
      "Like: `// [[ export_external(n = _) ]]`."
    )
  }

  n <- validate_n(n)

  args <- list(
    name = name,
    n = n
  )

  new_argument_df(attribute = "export_external", args = list(args))
}

hook_callable <- function(..., name = NA_character_, hidden = FALSE) {
  validate_no_dots(...)
  validate_name(name)

  if (!is.logical(hidden)) {
    class <- class_collapse(name)
    abort("`hidden` must be a logical, not a '", class, "'.")
  }

  if (is.na(hidden)) {
    abort("`hidden` must not be `NA`.")
  }

  args <- list(
    name = name,
    hidden = hidden
  )

  new_argument_df(attribute = "callable", args = list(args))
}

hook_init <- function(...) {
  validate_no_dots(...)
  args <- list()
  new_argument_df(attribute = "init", args = list(args))
}

# ------------------------------------------------------------------------------

class_collapse <- function(x) {
  paste(class(x), collapse = "/")
}

validate_name <- function(name) {
  if (!is.character(name)) {
    class <- class_collapse(name)
    abort("`name` must be a character, not a '", class, "'.")
  }

  if (length(name) != 1L) {
    abort("`name` must be length 1, not ", length(name), ".")
  }

  if (grepl("\\s", name)) {
    abort_problem_parsing(name, "`name` must not contain any spaces.")
  }

  invisible(name)
}

validate_n <- function(n) {
  if (length(n) != 1L) {
    abort("`n` must be size 1.")
  }

  if (!is.numeric(n)) {
    abort("`n` must be an integer value.")
  }

  n <- as.integer(n)

  if (isTRUE(n <= 0)) {
    abort("`n` must be greater than or equal to `0`.")
  }

  n
}

validate_no_dots <- function(...) {
  dots <- list(...)

  if (length(dots) == 0) {
    return(invisible())
  }

  nms <- names(dots)

  if (is.null(nms)) {
    msg <- "All arguments to an attribute function must be named."
  } else {
    nms <- paste0(double_quote(nms), collapse = ", ")

    msg <- paste0(
      "All arguments to an attribute function must be ",
      "named and spelled correctly. ",
      "Detected the following misspelled attribute argument names: ",
      nms
    )
  }

  abort(msg)
}
