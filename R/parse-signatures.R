parse_signatures <- function(attributes, lines) {
  attribute_types <- attributes$attribute

  rows_export <- which(attribute_types == "export")
  rows_export_external <- which(attribute_types == "export_external")
  rows_callable <- which(attribute_types == "callable")
  rows_init <- which(attribute_types == "init")

  attributes_export <- attributes[rows_export,]
  attributes_export_external <- attributes[rows_export_external,]
  attributes_callable <- attributes[rows_callable,]
  attributes_init <- attributes[rows_init,]

  signature <- vector("list", length(attribute_types))

  signature[rows_export] <- parse_signatures_export(attributes_export, lines)
  signature[rows_export_external] <- parse_signatures_export_external(attributes_export_external, lines)
  signature[rows_callable] <- parse_signatures_callable(attributes_callable, lines)
  signature[rows_init] <- parse_signatures_init(attributes_init, lines)

  attributes[["signature"]] <- signature

  attributes
}

# ------------------------------------------------------------------------------

parse_signatures_export <- function(attributes, lines) {
  if (nrow(attributes) == 0L) {
    return(list())
  }

  n_lines <- length(lines)

  attributes <- unnest_args(attributes)

  locs <- attributes$loc
  max_locs <- c(locs[-length(locs)], n_lines)
  names <- attributes$name

  # White space just causes problems at this point
  lines <- trimws(lines, "both")

  pmap(
    list(locs, max_locs, names),
    parse_signatures_export_line,
    lines = lines
  )
}

parse_signatures_export_line <- function(loc, max_loc, name, lines) {
  n_lines <- length(lines)

  loc_signature <- locate_signature_start(loc, max_loc, lines)
  signature <- lines[[loc_signature]]

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

  name_fn <- substr(signature, 1L, opening_parenthesis_loc - 1L)
  name_fn <- trimws(name_fn, which = "right")

  # Attribute name override with `export(name = value)`
  if (is.na(name)) {
    name_export <- name_fn
  } else {
    name_export <- name
  }

  # Trim off `(`
  signature <- substr(signature, opening_parenthesis_loc + 1L, nchar(signature))

  signature <- collect_signature_arguments(signature, loc_signature, max_loc, lines)

  args <- split_by_comma(signature)
  args <- parse_arguments(args)

  new_export_info(loc_signature, name_fn, name_export, args)
}

new_export_info <- function(loc, name, name_export, args) {
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

parse_signatures_export_external <- function(attributes, lines) {
  if (nrow(attributes) == 0L) {
    return(list())
  }

  n_lines <- length(lines)

  attributes <- unnest_args(attributes)

  locs <- attributes$loc
  max_locs <- c(locs[-length(locs)], n_lines)
  names <- attributes$name

  # White space just causes problems at this point
  lines <- trimws(lines, "both")

  pmap(
    list(locs, max_locs, names),
    parse_signatures_export_external_line,
    lines = lines
  )
}

parse_signatures_export_external_line <- function(loc, max_loc, name, lines) {
  n_lines <- length(lines)

  loc_signature <- locate_signature_start(loc, max_loc, lines)
  signature <- lines[[loc_signature]]

  # Does it start with `SEXP `?
  if (!starts_with_SEXP(signature)) {
    stop("The exported external function must have a return value of `SEXP`", call. = FALSE)
  }

  signature <- substr(signature, 6L, nchar(signature))

  # Locate `(`
  opening_parenthesis_loc <- locate_opening_parenthesis(signature)

  if (is.na(opening_parenthesis_loc)) {
    stop("Cannot find opening parenthesis.", call. = FALSE)
  }

  name_fn <- substr(signature, 1L, opening_parenthesis_loc - 1L)
  name_fn <- trimws(name_fn, which = "right")

  # Attribute name override with `export_external(name = value)`
  if (is.na(name)) {
    name_export <- name_fn
  } else {
    name_export <- name
  }

  # Trim off `(`
  signature <- substr(signature, opening_parenthesis_loc + 1L, nchar(signature))

  signature <- collect_signature_arguments(signature, loc_signature, max_loc, lines)

  args <- split_by_comma(signature)
  args <- parse_arguments(args)

  if (length(args) != 4L) {
    abort(
      ".External functions must have 4 arguments, ",
      "preferably named: ",
      "`call`, `op`, `args`, `env`."
    )
  }

  new_export_external_info(loc_signature, name_fn, name_export)
}

new_export_external_info <- function(loc, name, name_export) {
  list(
    name = name,
    name_export = name_export,
    loc = loc
  )
}

# ------------------------------------------------------------------------------

parse_signatures_callable <- function(attributes, lines) {
  if (nrow(attributes) == 0L) {
    return(list())
  }

  n_lines <- length(lines)

  attributes <- unnest_args(attributes)

  locs <- attributes$loc
  max_locs <- c(locs[-length(locs)], n_lines)
  names <- attributes$name

  # White space just causes problems at this point
  lines <- trimws(lines, "both")

  pmap(
    list(locs, max_locs, names),
    parse_signatures_callable_line,
    lines = lines
  )
}

parse_signatures_callable_line <- function(loc, max_loc, name, lines) {
  n_lines <- length(lines)

  loc_signature <- locate_signature_start(loc, max_loc, lines)
  signature <- lines[[loc_signature]]

  # Locate `(`
  opening_parenthesis_loc <- locate_opening_parenthesis(signature)

  if (is.na(opening_parenthesis_loc)) {
    stop("Cannot find opening parenthesis.", call. = FALSE)
  }

  # Locate the opening parenthesis and work backwards from that to find the
  # first space. This is the start of the function name
  name_and_return <- substr(signature, 1L, opening_parenthesis_loc - 1L)
  name_and_return <- trimws(name_and_return, which = "right")

  # TODO (this could definitely be better)
  # - flip name + return value
  # - locate first space (the start of the name)
  # - trim up to that to get the name
  # - rev back
  rev_name_and_return <- rev_chr(name_and_return)
  rev_name_end <- locate_text(" ", rev_name_and_return)
  rev_name <- substr(rev_name_and_return, 1L, rev_name_end - 1L)
  name_fn <- rev_chr(rev_name)

  # Attribute name override with `export(name = value)`
  if (is.na(name)) {
    name_callable <- name_fn
  } else {
    name_callable <- name
  }

  # Trim off `(`
  signature <- substr(signature, opening_parenthesis_loc + 1L, nchar(signature))

  signature <- collect_signature_arguments(signature, loc_signature, max_loc, lines)

  args <- split_by_comma(signature)
  args <- parse_arguments(args)

  new_callable_info(loc_signature, name_fn, name_callable, args)
}

new_callable_info <- function(loc, name, name_callable, args) {
  n_args <- length(args)

  list(
    name = name,
    name_callable = name_callable,
    args = args,
    n_args = n_args,
    loc = loc
  )
}

rev_chr <- function(x) {
  paste0(rev(strsplit(x, "")[[1]]), collapse = "")
}

# ------------------------------------------------------------------------------

parse_signatures_init <- function(attributes, lines) {
  if (nrow(attributes) == 0L) {
    return(list())
  }

  n_lines <- length(lines)

  attributes <- unnest_args(attributes)

  locs <- attributes$loc
  max_locs <- c(locs[-length(locs)], n_lines)

  # White space just causes problems at this point
  lines <- trimws(lines, "both")

  pmap(
    list(locs, max_locs),
    parse_signatures_init_line,
    lines = lines
  )
}

parse_signatures_init_line <- function(loc, max_loc, lines) {
  n_lines <- length(lines)

  loc_signature <- locate_signature_start(loc, max_loc, lines)
  signature <- lines[[loc_signature]]

  # Does it start with `void `?
  if (!substr(signature, 1L, 5L) == "void ") {
    stop("The init function must have a return value of `void`", call. = FALSE)
  }

  signature <- substr(signature, 6L, nchar(signature))

  # Locate `(`
  opening_parenthesis_loc <- locate_opening_parenthesis(signature)

  if (is.na(opening_parenthesis_loc)) {
    stop("Cannot find opening parenthesis.", call. = FALSE)
  }

  name <- substr(signature, 1L, opening_parenthesis_loc - 1L)
  name <- trimws(name, which = "right")

  # Trim off `(`
  signature <- substr(signature, opening_parenthesis_loc + 1L, nchar(signature))

  signature <- collect_signature_arguments(signature, loc_signature, max_loc, lines)

  args <- split_by_comma(signature)

  if (length(args) != 1L) {
    abort("There can only be 1 argument for an init function, a `DllInfo*`.")
  }

  # Some people do `DllInfo *dll`, so remove all spaces and find the `*`
  args <- sub(" ", "", args)
  loc_dllinfo_endpoint <- locate_text("*", args)

  if (is.na(loc_dllinfo_endpoint)) {
    abort("The only argument allowed for an init function is a `DllInfo*`.")
  }

  args <- substr(args, 1L, loc_dllinfo_endpoint - 1L)

  if (!identical(args, "DllInfo")) {
    abort("The only argument allowed for an init function is a `DllInfo*`.")
  }

  new_init_info(loc_signature, name)
}

new_init_info <- function(loc, name) {
  list(
    name = name,
    loc = loc
  )
}

# ------------------------------------------------------------------------------

locate_signature_start <- function(loc, max_loc, lines) {
  loc <- loc + 1L
  signature <- lines[[loc]]

  while(startsWith(signature, "//")) {
    if (loc == max_loc) {
      abort("Detected a cbuild attribute, but found no function to go along with it.")
    }

    loc <- loc + 1L
    signature <- lines[[loc]]
  }

  loc
}

collect_signature_arguments <- function(signature, loc, max_loc, lines) {
  # Locate `)`
  closing_parenthesis_loc <- locate_closing_parenthesis(signature)

  # Find `)` if it is over multiple lines
  while(is.na(closing_parenthesis_loc)) {
    if (loc == max_loc) {
      stop("Cannot find closing parenthesis", call. = FALSE)
    }

    loc <- loc + 1L

    partial_signature <- lines[[loc]]
    partial_signature <- trimws(partial_signature, "both")

    signature <- paste(signature, partial_signature)
    closing_parenthesis_loc <- locate_closing_parenthesis(signature)
  }

  # Trim off everything at and after `)`
  signature <- substr(signature, 1L, closing_parenthesis_loc - 1L)

  signature
}
