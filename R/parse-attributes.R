parse_attributes <- function(lines) {
  lines <- trimws(lines, "both")

  # Line numbers are tracked as we locate attributes
  names(lines) <- seq_along(lines)

  # Detect and remove `//` and any whitespace after it
  lines <- lines[startsWith(lines, "//")]
  lines <- substr(lines, 3L, nchar(lines))
  lines <- trimws(lines, "left")
  lines <- lines[lines != ""]

  # Detect and remove `[[` and any whitespace after it
  lines <- lines[startsWith(lines, "[[")]
  lines <- substr(lines, 3L, nchar(lines))
  lines <- trimws(lines, "left")
  lines <- lines[lines != ""]

  # Detect and remove `]]` and any whitespace before it
  lines <- lines[endsWith(lines, "]]")]
  lines <- substr(lines, 1L, nchar(lines) - 2L)
  lines <- trimws(lines, "right")
  lines <- lines[lines != ""]

  # Pull the line number locations off
  locs <- as.numeric(names(lines))
  lines <- unname(lines)

  # Separate each potential attribute left by `;`
  attributes <- strsplit(lines, ";", fixed = TRUE)

  # Go ahead and remove whitespace
  attributes <- map(attributes, trimws, which = "both")

  if (length(attributes) == 0L) {
    return(new_attribute_df())
  }

  lst_of_attribute_dfs <- map(attributes, eval_line_attributes)

  lst_of_attribute_dfs <- map2(locs, lst_of_attribute_dfs, bind_loc)

  attribute_df <- do.call(rbind, lst_of_attribute_dfs)

  attribute_df
}

bind_loc <- function(loc, attribute_df) {
  cbind(
    data.frame(loc = loc),
    attribute_df
  )
}

eval_line_attributes <- function(x) {
  lst_of_attribute_dfs <- map(x, eval_attribute)
  do.call(rbind, lst_of_attribute_dfs)
}

eval_attribute <- function(x) {
  expr <- parse(text = x)

  out <- eval(expr, envir = cbuild__hook_env, enclos = baseenv())

  validate_called_with_parenthesis(out, x)

  out
}

# ------------------------------------------------------------------------------

validate_called_with_parenthesis <- function(x, text) {
  is_identical <- eapply(cbuild__hook_env, identical, x)
  is_identical <- unlist(is_identical, recursive = FALSE, use.names = FALSE)

  if (!any(is_identical)) {
    return(invisible(x))
  }

  msg <- paste0(
    "Attributes should be specified as valid R function calls, ",
    "like `export()`, not `export`."
  )

  abort_problem_parsing(text, msg)
}

# ------------------------------------------------------------------------------

locate_text <- function(text, line) {
  out <- regexpr(text, line, fixed = TRUE)
  out[out == -1L] <- NA_integer_
  as.integer(out)
}

locate_opening_parenthesis <- function(line) {
  locate_text("(", line)
}

locate_closing_parenthesis <- function(line) {
  locate_text(")", line)
}
