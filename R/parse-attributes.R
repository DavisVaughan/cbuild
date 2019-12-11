locate_and_parse_attributes <- function(lines) {
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

  # Separate each potential attribute left by `&&`
  attributes <- strsplit(lines, "&&", fixed = TRUE)

  # Now run each attribute currently in a form like
  # `attr(name = value)` through the parser
  attributes <- map(attributes, parse_all_attributes)

  out <- data.frame(loc = locs)
  out[["attributes"]] <- attributes

  out
}

parse_all_attributes <- function(x) {
  x <- trimws(x, "both")
  names <- parse_all_attribute_names(x)
  out <- parse_all_attribute_arguments(x)
  names(out) <- names
  out
}

parse_all_attribute_names <- function(x) {
  out <- vector("character", length(x))

  opening_parenthesis_loc <- locate_opening_parenthesis(x)

  # Assume its just the name if no opening `(`
  just_the_name <- is.na(opening_parenthesis_loc)
  out[just_the_name] <- x[just_the_name]

  x <- x[!just_the_name]
  opening_parenthesis_loc <- opening_parenthesis_loc[!just_the_name]

  names <- substr(x, 1L, opening_parenthesis_loc - 1L)
  names <- trimws(names, "right")

  out[!just_the_name] <- names

  out
}

parse_all_attribute_arguments <- function(x) {
  out <- vector("list", length(x))

  opening_parenthesis_loc <- locate_opening_parenthesis(x)

  just_the_name <- is.na(opening_parenthesis_loc)
  empty_attribute_df <- data.frame(name = character(), value = character(), stringsAsFactors = FALSE)
  out[just_the_name] <- list(empty_attribute_df)

  x <- x[!just_the_name]
  opening_parenthesis_loc <- opening_parenthesis_loc[!just_the_name]

  closing_parenthesis_loc <- locate_closing_parenthesis(x)

  if (any(is.na(closing_parenthesis_loc))) {
    problem_loc <- which(is.na(closing_parenthesis_loc))[[1]]
    problem <- x[[problem_loc]]

    stop(
      "Problem parsing '",
      problem,
      "', cannot have an opening parenthesis without a closing one.",
      call. = FALSE
    )
  }

  # Get the args between the parenthesis
  x <- substr(x, opening_parenthesis_loc + 1L, closing_parenthesis_loc - 1L)

  # Split them by `,`
  x <- strsplit(x, ",", fixed = TRUE)
  x <- map(x, trimws, "both")

  out[!just_the_name] <- map(x, parse_single_attribute_arguments)

  out
}

parse_single_attribute_arguments <- function(x) {
  equals_loc <- regexpr("=", x, fixed = TRUE)

  if (any(equals_loc == -1L)) {
    problem_loc <- which(equals_loc == -1L)[[1]]
    problem <- x[[problem_loc]]
    stop("All attribute arguments must be named, like `name = value`. Parsing: ", problem, call. = FALSE)
  }

  name <- substr(x, 1L, equals_loc - 1L)
  name <- trimws(name, "right")

  value <- substr(x, equals_loc + 1L, nchar(x))
  value <- trimws(value, "left")

  if (any(nchar(name) == 0L)) {
    problem_loc <- which(nchar(name) == 0L)[[1]]
    problem <- name[[problem_loc]]
    stop("The attribute argument name has length 0. It must have a name. Parsing: ", problem, call. = FALSE)
  }

  if (any(nchar(value) == 0L)) {
    problem_loc <- which(nchar(value) == 0L)[[1]]
    problem <- value[[problem_loc]]
    stop("The attribute argument value has length 0. It must have a value. Parsing: ", problem, call. = FALSE)
  }

  data.frame(name = name, value = value, stringsAsFactors = FALSE)
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
