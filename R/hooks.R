hook_export <- function(name = NA_character_, type = "call") {
  validate_name(name)

  if (!identical(type, "call") && !identical(type, "external")) {
    abort("`type` must be either 'call' or 'external'.")
  }

  args <- list(
    name = name,
    type = type
  )

  new_argument_df(type = "export", args = list(args))
}

hook_callable <- function(name = NA_character_, hidden = FALSE) {
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

  new_argument_df(type = "callable", args = list(args))
}

hook_init <- function() {
  args <- list()
  new_argument_df(type = "init", args = list(args))
}

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
