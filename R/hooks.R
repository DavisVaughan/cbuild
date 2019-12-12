hook_export <- function(name = NA_character_, type = "call") {
  if (!is.character(name)) {
    class <- class_collapse(name)
    abort("`name` must be `NULL` or a character, not a '", class, "'.")
  }

  if (length(name) != 1L) {
    abort("`name` must be length 1, not ", length(name), ".")
  }

  if (!identical(type, "call") && !identical(type, "external")) {
    abort("`type` must be either 'call' or 'external'.")
  }

  args <- list(
    name = name,
    type = type
  )

  new_argument_df(type = "export", args = list(args))
}

hook_register <- function() {
  new_argument_df(type = "register", args = list(list()))
}

class_collapse <- function(x) {
  paste(class(x), collapse = "/")
}
