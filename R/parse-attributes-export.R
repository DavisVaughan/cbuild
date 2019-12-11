# Used for `source_*()` functions, which just care about `export` attributes
locate_and_parse_export_attributes <- function(lines) {
  attribute_df <- locate_and_parse_attributes(lines)

  attributes <- attribute_df$attributes
  attributes <- map(attributes, function(x) x[names(x) == "export"])

  has_export <- map_lgl(attributes, function(x) length(x) == 1L)

  attributes <- attributes[has_export]
  exports <- map(attributes, function(x) x[[1L]])

  # TODO - Don't allow `exports(type = external)`

  locs <- attribute_df$loc[has_export]

  data_frame(loc = locs, exports = exports)
}

parse_exports <- function(lines) {
  n_lines <- length(lines)

  # Whitespace just causes issues
  lines <- trimws(lines, "both")

  attributes <- locate_and_parse_export_attributes(lines)
  locs <- attributes$loc
  exports <- attributes$exports

  n_exports <- length(locs)

  if (n_exports == 0L) {
    stop("At least 1 function must be marked for export", call. = FALSE)
  }

  out <- vector("list", length = n_exports)

  signature_locs <- locs + 1L

  for (i in seq_len(n_exports)) {
    loc <- signature_locs[[i]]
    signature <- lines[[loc]]

    # Skip over comments that might be between the attribute and the
    # start of the function
    skip <- FALSE
    while(startsWith(signature, "//")) {
      if (i == n_exports) {
        next_loc <- n_lines
      } else {
        next_loc <- signature_locs[[i + 1L]]
      }

      if (loc == next_loc) {
        skip <- TRUE
        break
      }

      loc <- loc + 1L
      signature <- lines[[loc]]
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

    # Attribute name override with `export(name = value)`
    name_export <- name

    export <- exports[[i]]
    has_name_override <- "name" == export$name

    if (any(has_name_override)) {
      name_row <- which(has_name_override)[[1]]
      name_export <- export$value[[name_row]]
    }

    if (isTRUE(grepl("\\s", name))) {
      stop("The exported function cannot have any spaces in its name", call. = FALSE)
    }

    # Trim off `(`
    signature <- substr(signature, opening_parenthesis_loc + 1L, nchar(signature))

    # Locate `)`
    closing_parenthesis_loc <- locate_closing_parenthesis(signature)

    # Find `)` if it is over multiple lines
    loc_temp <- loc
    while(is.na(closing_parenthesis_loc)) {
      if (i == n_exports) {
        next_loc <- n_lines
      } else {
        next_loc <- signature_locs[[i + 1L]]
      }

      if (loc_temp == next_loc) {
        stop("Cannot find closing parenthesis", call. = FALSE)
      }

      loc_temp <- loc_temp + 1L

      partial_signature <- lines[[loc_temp]]
      partial_signature <- trimws(partial_signature, "both")

      signature <- paste(signature, partial_signature)
      closing_parenthesis_loc <- locate_closing_parenthesis(signature)
    }

    # Trim off everything at and after `)`
    signature <- substr(signature, 1L, closing_parenthesis_loc - 1L)

    args <- split_by_comma(signature)
    args <- parse_arguments(args)

    out[[i]] <- new_function_info(loc, name, name_export, args)
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

split_by_comma <- function(x) {
  strsplit(x, ",", fixed = TRUE)[[1]]
}

starts_with_SEXP <- function(x) {
  substr(x, 1L, 5L) == "SEXP "
}

# name = character(1) of the original function name
# args = character(n) of the function names
new_function_info <- function(loc, name, name_export, args) {
  n_args <- length(args)

  list(
    name = name,
    name_export = name_export,
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

    name_export <- paste0("cbuild_", fn_info$name_export)

    signature <- gsub(fn_info$name, name_export, signature, fixed = TRUE)
    lines[[fn_info$loc]] <- signature
  }

  lines
}



