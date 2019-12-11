parse_exports <- function(lines) {
  n_lines <- length(lines)

  # Whitespace just causes issues
  lines <- trimws(lines, "both")

  tags <- locate_tags(lines)
  n_exports <- length(tags$exports)

  if (n_exports == 0L) {
    stop("At least 1 function must be marked for export", call. = FALSE)
  }

  out <- vector("list", length = n_exports)

  signature_locs <- tags$exports + 1L

  for (i in seq_len(n_exports)) {
    loc <- signature_locs[[i]]
    signature <- lines[[loc]]

    # Skip over comments that might be between the attribute and the
    # start of the function
    skip <- FALSE
    while(startsWith(signature, "//")) {
      loc <- loc + 1L
      signature <- lines[[loc]]

      if (i == n_exports) {
        next_loc <- n_lines
      } else {
        next_loc <- signature_locs[[i + 1L]]
      }

      if (loc == next_loc) {
        skip <- TRUE
        break
      }
    }

    if (skip) {
      next
    }

    # Does it start with `SEXP `?
    if (!starts_with_SEXP(signature)) {
      stop("The exported function must have a return value of `SEXP`", call. = FALSE)
    }

    signature <- substr(signature, 6L, nchar(signature))

    # Locate `(`
    opening_parenthesis_loc <- locate_opening_parenthesis(signature)

    if (is.na(opening_parenthesis_loc)) {
      stop("Cannot find opening parenthesis.", call. = FALSE)
    }

    name <- substr(signature, 1L, opening_parenthesis_loc - 1L)
    name <- trimws(name, which = "right")

    if (isTRUE(grepl("\\s", name))) {
      stop("The exported function cannot have any spaces in its name", call. = FALSE)
    }

    # Trim off `(`
    signature <- substr(signature, opening_parenthesis_loc + 1L, nchar(signature))

    # Locate `)`
    # TODO - maybe make this more flexible (args over multiple lines)
    closing_parenthesis_loc <- locate_closing_parenthesis(signature)

    if (is.na(closing_parenthesis_loc)) {
      stop("Cannot find closing parenthesis.", call. = FALSE)
    }

    # Trim off everything at and after `)`
    signature <- substr(signature, 1L, closing_parenthesis_loc - 1L)

    args <- split_by_comma(signature)
    args <- parse_arguments(args)

    out[[i]] <- new_function_info(loc, name, args)
  }

  out
}

# ------------------------------------------------------------------------------

parse_arguments <- function(args) {
  args <- trimws(args, which = "both")

  # Does it start with `SEXP `?
  if (any(!starts_with_SEXP(args))) {
    stop("The exported function's arguments must all be `SEXP`s", call. = FALSE)
  }

  args <- substr(args, 6L, nchar(args))

  if (any(grepl("\\s", args))) {
    stop("The exported function's arguments cannot have any spaces in their names", call. = FALSE)
  }

  args
}

# ------------------------------------------------------------------------------

locate_tags <- function(lines) {
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

  # Separate each potential attribute left by `,` and detect known attributes
  attributes <- strsplit(lines, ",", fixed = TRUE)
  attributes <- map(attributes, trimws, which = "both")

  has_export <- map_lgl(attributes, has_attribute, attribute = "cbuild::export")
  exports <- locs[has_export]

  has_external <- map_lgl(attributes, has_attribute, attribute = "cbuild::external")
  external <- locs[has_external]

  list(
    exports = exports,
    external = external
  )
}

has_attribute <- function(x, attribute) {
  any(attribute %in% x)
}

locate_text <- function(text, line) {
  out <- regexpr(text, line, fixed = TRUE)

  if (length(out) != 1L) {
    stop("Internal error: `line` should have been size one.")
  }

  if (out == -1L) {
    NA_integer_
  } else {
    as.integer(out)
  }
}

locate_opening_parenthesis <- function(line) {
  locate_text("(", line)
}

locate_closing_parenthesis <- function(line) {
  locate_text(")", line)
}

split_by_comma <- function(x) {
  strsplit(x, ",", fixed = TRUE)[[1]]
}

starts_with_SEXP <- function(x) {
  substr(x, 1L, 5L) == "SEXP "
}

# name = character(1) of the original function name
# args = character(n) of the function names
new_function_info <- function(loc, name, args) {
  n_args <- length(args)
  name_symbol <- paste0("cbuild_", name)

  list(
    name = name,
    name_symbol = name_symbol,
    args = args,
    n_args = n_args,
    loc = loc
  )
}

# ------------------------------------------------------------------------------

# Replace all exported function names with `cbuild_<fn_name>` so we can
# bind the symbols to R functions of the name `<fn_name>`
replace_function_names <- function(lines, info) {
  n_functions <- length(info)

  for (i in seq_len(n_functions)) {
    fn_info <- info[[i]]
    signature <- lines[[fn_info$loc]]
    signature <- gsub(fn_info$name, fn_info$name_symbol, signature, fixed = TRUE)
    lines[[fn_info$loc]] <- signature
  }

  lines
}



