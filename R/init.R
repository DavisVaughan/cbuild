write_init <- function(path, pkg, info, debug) {
  if (!can_write_init(path)) {
    return(new_init_result())
  }

  lines <- character()

  lines <- write_do_not_modify(lines)
  lines <- write_init_includes(lines)
  lines <- write_call_exports_and_entries(lines, info)
  lines <- write_external_exports_and_entries(lines, info)
  lines <- write_callables(lines, info, hidden = FALSE)
  lines <- write_callables(lines, info, hidden = TRUE)
  lines <- write_init_hook_declarations(lines, info)
  lines <- write_r_init_pkg(lines, info, pkg)

  out <- new_init_result(lines)

  if (debug) {
    return(out)
  }

  path_init <- path_init(path)
  remove_preexiting_init(path_init)
  create_new_init(path_init)
  write_lines(path_init, lines)

  out
}

# ------------------------------------------------------------------------------

write_do_not_modify <- function(lines) {
  c(lines, do_not_modify(), new_line(), new_line())
}

do_not_modify <- function() {
  "// File generated automatically by cbuild - please do not modify by hand"
}

# ------------------------------------------------------------------------------

write_init_includes <- function(lines) {
  c(
    lines,
    "#include <R.h>",
    new_line(),
    "#include <Rinternals.h>",
    new_line(),
    "#include <stdlib.h> // for NULL",
    new_line(),
    "#include <stdbool.h> // for bool",
    new_line(),
    "#include <R_ext/Rdynload.h>",
    new_line(),
    new_line()
  )
}

# ------------------------------------------------------------------------------

write_call_exports_and_entries <- function(lines, info) {
  info <- info[info$attribute == "export",]
  info <- unnest_args(info)

  if (nrow(info) == 0L) {
    return(lines)
  }

  signatures <- info$signature

  lines <- write_call_exports(lines, signatures)
  lines <- write_call_entries(lines, signatures)

  lines
}

write_call_exports <- function(lines, signatures) {
  names <- map_chr(signatures, function(x) x$name)

  n_args <- map_int(signatures, function(x) x$n_args)
  sexp_arg_list <- map_chr(n_args, make_sexp_arg_list)

  declarations <- paste0("extern SEXP ", names, "(", sexp_arg_list, ");")

  lines <- add_lines(lines, "// .Call declarations")
  lines <- add_lines(lines, declarations)
  lines <- c(lines, new_line())

  lines
}

write_call_entries <- function(lines, signatures) {
  names <- map_chr(signatures, function(x) x$name)
  names_export <- map_chr(signatures, function(x) x$name_export)
  names_export <- double_quote(names_export)
  n_args <- map_int(signatures, function(x) x$n_args)

  padding <- compute_padding(names_export)

  header <- "static const R_CallMethodDef CallEntries[] = {"
  entries <- paste0("  {", names_export, ",", padding, "(DL_FUNC) &", names, ", ", n_args, "},")
  ender <- "  {NULL, NULL, 0}"
  footer <- "};"

  lines <- add_lines(lines, "// .Call entries")
  lines <- add_lines(lines, header)
  lines <- add_lines(lines, entries)
  lines <- add_lines(lines, ender)
  lines <- add_lines(lines, footer)

  lines <- c(lines, new_line())

  lines
}

# ------------------------------------------------------------------------------

write_external_exports_and_entries <- function(lines, info) {
  lines <- write_external_exports(lines, info, two = FALSE)
  lines <- write_external_exports(lines, info, two = TRUE)
  lines <- write_external_entries(lines, info)
  lines
}

write_external_exports <- function(lines, info, two = FALSE) {
  if (two) {
    type <- "export_external2"
    n_sexps <- 4L
    comment <- "// .External2 declarations"
  } else {
    type <- "export_external"
    n_sexps <- 1L
    comment <- "// .External declarations"
  }

  info <- info[info$attribute == type,]

  if (nrow(info) == 0L) {
    return(lines)
  }

  info <- unnest_args(info)

  signatures <- info$signature

  names <- map_chr(signatures, function(x) x$name)

  n_args <- rep(n_sexps, length(signatures))
  sexp_arg_list <- map_chr(n_args, make_sexp_arg_list)

  declarations <- paste0("extern SEXP ", names, "(", sexp_arg_list, ");")

  lines <- add_lines(lines, comment)
  lines <- add_lines(lines, declarations)
  lines <- c(lines, new_line())

  lines
}

write_external_entries <- function(lines, info) {
  all_external <- info$attribute == "export_external" |
    info$attribute == "export_external2"

  info <- info[all_external,]

  if (nrow(info) == 0L) {
    return(lines)
  }

  header <- "static const R_ExternalMethodDef ExtEntries[] = {"
  ender <- "  {NULL, NULL, 0}"
  footer <- "};"

  lines <- add_lines(lines, "// .External / .External2 entries")
  lines <- add_lines(lines, header)
  lines <- add_external_entries(lines, info)
  lines <- add_lines(lines, ender)
  lines <- add_lines(lines, footer)
  lines <- c(lines, new_line())

  lines
}

add_external_entries <- function(lines, info) {
  if (nrow(info) == 0L) {
    return(lines)
  }

  info <- unnest_args(info)
  signatures <- info$signature

  names <- map_chr(signatures, function(x) x$name)
  names_export <- map_chr(signatures, function(x) x$name_export)
  names_export <- double_quote(names_export)

  n <- info$n

  padding <- compute_padding(names_export)

  entries <- paste0("  {", names_export, ",", padding, "(DL_FUNC) &", names, ", ", n, "},")

  lines <- add_lines(lines, entries)

  lines
}

# ------------------------------------------------------------------------------

write_callables <- function(lines, info, hidden) {
  callable <- info[info$attribute == "callable",]
  is_hidden <- map_lgl(callable$args, function(x) x$hidden)

  if (!hidden) {
    is_hidden <- !is_hidden
  }

  callable <- callable[is_hidden,]

  if (nrow(callable) == 0L) {
    return(lines)
  }

  # Ensure callables have not already been declared by .Call or .External2/.External
  info <- remove_exports(callable, info)

  if (nrow(info) == 0L) {
    return(lines)
  }

  signatures <- info$signature

  names <- map_chr(signatures, function(x) x$name)

  types <- map(signatures, function(x) x$arg_types)
  types <- map(types, function(x) paste0(x, collapse = ", "))

  return <- map_chr(signatures, function(x) x$return)

  declarations <- paste0("extern ", return, " ", names, "(", types, ");")

  if (hidden) {
    lines <- add_lines(lines, "// Hidden callable API declarations")
  } else {
    lines <- add_lines(lines, "// Callable API declarations")
  }

  lines <- add_lines(lines, declarations)
  lines <- c(lines, new_line())

  lines
}

remove_exports <- function(callable, info) {
  loc_all_exports <- info$attribute == "export" |
    info$attribute == "export_external" |
    info$attribute == "export_external2"

  exports <- info[loc_all_exports,]

  if (nrow(exports) == 0L) {
    return(callable)
  }

  exports_id <- paste0(exports$file, "_", exports$loc)
  callable_id <- paste0(callable$file, "_", callable$loc)

  matches <- match(exports_id, callable_id)
  matches <- matches[!is.na(matches)]

  if (length(matches) == 0L) {
    callable
  } else {
    callable[-matches,]
  }
}

# ------------------------------------------------------------------------------

write_init_hook_declarations <- function(lines, info) {
  info <- info[info$attribute == "init",]

  if (nrow(info) == 0L) {
    return(lines)
  }

  signatures <- info$signature

  names <- map_chr(signatures, function(x) x$name)

  init_functions <- paste0("void ", names, "(DllInfo* dll);")

  lines <- add_lines(lines, "// Init hook declarations")
  lines <- add_lines(lines, init_functions)
  lines <- c(lines, new_line())
  lines
}

# ------------------------------------------------------------------------------

write_r_init_pkg <- function(lines, info, pkg) {
  if (has_exports(info, "export")) {
    call_entries  <- "CallEntries"
  } else {
    call_entries <- "NULL"
  }

  if (has_exports(info, "export_external") | has_exports(info, "export_external2")) {
    external_entries <- "ExtEntries"
  } else {
    external_entries <- "NULL"
  }

  header <- paste0("void R_init_", pkg, "(DllInfo *dll) {")
  footer <- "}"

  register_routines <- paste0(
    "  R_registerRoutines(dll, NULL, ", call_entries, ", NULL, ", external_entries, ");"
  )

  dynamic_symbols <- "  R_useDynamicSymbols(dll, FALSE);"

  lines <- add_lines(lines, header)
  lines <- add_lines(lines, register_routines)
  lines <- add_lines(lines, dynamic_symbols)

  lines <- write_register_callables(lines, info, pkg, hidden = TRUE)
  lines <- write_register_callables(lines, info, pkg, hidden = FALSE)

  lines <- write_init_functions(lines, info)

  lines <- add_lines(lines, footer)
  lines <- c(lines, new_line())

  lines
}

write_init_functions <- function(lines, info) {
  info <- info[info$attribute == "init",]

  if (nrow(info) == 0L) {
    return(lines)
  }

  signatures <- info$signature

  names <- map_chr(signatures, function(x) x$name)

  init_functions <- paste0("  ", names, "(dll);")

  lines <- c(lines, new_line())
  lines <- add_lines(lines, init_functions)
  lines
}

write_register_callables <- function(lines, info, pkg, hidden) {
  info <- info[info$attribute == "callable",]
  is_hidden <- map_lgl(info$args, function(x) x$hidden)

  if (!hidden) {
    is_hidden <- !is_hidden
  }

  info <- info[is_hidden,]

  if (nrow(info) == 0L) {
    return(lines)
  }

  signatures <- info$signature

  names <- map_chr(signatures, function(x) x$name)
  name_callable <- map_chr(signatures, function(x) x$name_callable)

  registrations <- paste0(
    "  R_RegisterCCallable(",
    double_quote(pkg),
    ", ",
    double_quote(name_callable),
    ",",
    compute_padding(name_callable),
    "(DL_FUNC) &",
    names,
    ");"
  )

  lines <- c(lines, new_line())

  if (hidden) {
    lines <- add_lines(lines, "  // Hidden callable API registrations")
  } else {
    lines <- add_lines(lines, "  // Callable API registrations")
  }

  lines <- add_lines(lines, registrations)

  lines
}

# ------------------------------------------------------------------------------

collect_attributes_and_signatures <- function(path) {
  dir_src <- dir_src(path)

  # TODO should it be recursive? rlang?
  path_src_files <- list.files(
    dir_src,
    pattern = utils::glob2rx("*.c"),
    recursive = FALSE,
    full.names = TRUE
  )

  # Remove `init.c` file
  loc_init_c <- grepl(utils::glob2rx("*/init.c"), path_src_files)
  path_src_files <- path_src_files[!loc_init_c]

  if (length(path_src_files) == 0L) {
    return(new_signature_df())
  }

  lst_of_attribute_df <- map(path_src_files, parse_attributes_and_signatures_in_file)

  name_src_files <- basename(path_src_files)

  lst_of_attribute_df <- map2(name_src_files, lst_of_attribute_df, bind_file_name)

  do.call(rbind, lst_of_attribute_df)
}

bind_file_name <- function(file, attribute_df) {
  if (nrow(attribute_df) == 0L) {
    file_df <- data_frame(file = character())
  } else {
    file_df <- data_frame(file = file)
  }

  cbind(file_df, attribute_df)
}

parse_attributes_and_signatures_in_file <- function(file) {
  lines <- read_lines(file)

  info <- parse_attributes(lines)
  info <- parse_signatures(info, lines)

  info
}

# ------------------------------------------------------------------------------

create_new_init <- function(path_init) {
  success <- file.create(path_init)

  if (!success) {
    abort("The creation of the fresh `init.c` file was not successful.")
  }

  invisible()
}

remove_preexiting_init <- function(path_init) {
  if (!file.exists(path_init)) {
    return(invisible())
  }

  success <- file.remove(path_init)

  if (!success) {
    abort("The removal of the `init.c` file was not successful.")
  }

  invisible()
}

can_write_init <- function(path) {
  path_init <- path_init(path)

  if (!file.exists(path_init)) {
    return(TRUE)
  }

  first_line <- readLines(path_init, n = 1L)

  made_by_cbuild <- identical(
    first_line,
    do_not_modify()
  )

  if (made_by_cbuild) {
    return(TRUE)
  }

  cat_line(
    "`init.c` file exists, but was not made by cbuild. ",
    "Is it okay to overwrite it?"
  )

  answer <- utils::menu(c("Yes", "No"))

  if (answer == 0L || answer == 2L) {
    FALSE
  } else {
    TRUE
  }
}

# ------------------------------------------------------------------------------

has_exports <- function(info, type) {
  info <- info[info$attribute == type,]

  if (nrow(info) == 0L) {
    FALSE
  } else {
    TRUE
  }
}

package_name <- function(path) {
  path_desc <- file.path(path, "DESCRIPTION")

  if (!file.exists(path_desc)) {
    abort("A `DESCRIPTION` file must exist in the package.")
  }

  pkg <- read.dcf(path_desc, fields = "Package")
  pkg <- as.vector(pkg)

  if (is.na(pkg)) {
    abort("The package `DESCRIPTION` file must include a package name.")
  }

  pkg
}

compute_padding <- function(x) {
  chars <- nchar(x)
  widest <- max(chars)

  n_padding <- widest + 1L - chars

  padding <- map_chr(n_padding, pad_dup)

  padding
}

pad_dup <- function(times) {
  paste0(rep(" ", times = times), collapse = "")
}

make_sexp_arg_list <- function(n) {
  if (n == 0L) {
    return("")
  }

  out <- "SEXP"

  if (n == 1L) {
    return(out)
  }

  out <- c(out, rep(", SEXP", n - 1L))
  out <- paste0(out, collapse = "")

  out
}

add_lines <- function(lines, ...) {
  additions <- c(...)
  new_lines <- rep(new_line(), length(additions))

  additions <- interleave(additions, new_lines)

  c(lines, additions)
}

interleave <- function(x, y) {
  idx <- order(c(seq_along(x), seq_along(y)))
  c(x, y)[idx]
}

new_line <- function() {
  "\n"
}

dir_src <- function(path) {
  file.path(path, "src")
}

path_init <- function(path) {
  file.path(dir_src(path), "init.c")
}

has_src <- function(path) {
  dir.exists(dir_src(path))
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

new_init_result <- function(init = character()) {
  list(init = init)
}
