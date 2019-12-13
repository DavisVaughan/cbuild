hook_export <- function(name = NA_character_) {
  validate_name(name)

  args <- list(
    name = name
  )

  new_argument_df(attribute = "export", args = list(args))
}

hook_export_external <- function(n, name = NA_character_) {
  validate_name(name)

  if (missing(n)) {
    abort(
      ".External functions require the `n` argument. ",
      "Like: `// [[ export_external(n = _) ]]`."
    )
  }

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

  args <- list(
    name = name,
    n = n
  )

  new_argument_df(attribute = "export_external", args = list(args))
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

  new_argument_df(attribute = "callable", args = list(args))
}

hook_init <- function() {
  args <- list()
  new_argument_df(attribute = "init", args = list(args))
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
