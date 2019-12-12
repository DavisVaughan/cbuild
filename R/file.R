#' Source a file containing C code
#'
#' `source_file()` will parse through `file` looking for functions tagged with
#' `// [[ export() ]]` and will compile the file and export those functions to
#' the R side.
#'
#' @param file `[character(1)]`
#'
#'   The C file to source.
#'
#' @param includes `[NULL / character()]`
#'
#'   Extra includes to add manually. By default, `R.h` and `Rinternals.h` are
#'   included. Specify more includes with their file name. For example, to
#'   include `#include <Rdefines.h>` you just need to specify `"Rdefines.h"`.
#'
#' @param no_remap `[logical(1)]`
#'
#'   Should `#define R_NO_REMAP` be defined?
#'
#' @param show `[logical(1)]`
#'
#'   Should the output of compiling the source code with `R CMD SHLIB` be shown?
#'
#' @return
#' A named list containing the functions specified for export.
#'
#' @examples
#' tf <- tempfile(fileext = ".c")
#'
#' code <- "
#'   // [[ export() ]]
#'   SEXP fn(SEXP x) {
#'     return x;
#'   }
#' "
#'
#' writeLines(code, tf)
#'
#' sourced <- source_file(tf)
#'
#' sourced$fn(1)
#' @export
source_file <- function(file, includes = NULL, no_remap = TRUE, show = FALSE) {
  file <- normalizePath(file, mustWork = TRUE)

  lines <- read_lines(file)

  info <- parse_exports(lines)

  lines <- replace_function_names(lines, info)

  lines <- add_default_includes(lines)

  if (!is.null(includes)) {
    lines <- add_provided_includes(lines, includes)
  }

  if (no_remap) {
    lines <- add_no_remap(lines)
  }

  dir_tmp <- tempdir()
  dir_cbuild <- file.path(dir_tmp, "cbuild")

  if (!dir.exists(dir_cbuild)) {
    dir.create(dir_cbuild)
  }

  path_src <- tempfile("cbuild_", tmpdir = dir_cbuild, fileext = ".c")
  path_so <- tempfile("cbuild_", tmpdir = dir_cbuild, fileext = .Platform$dynlib.ext)

  # Can only reliably clean up the src file. On windows you can't remove the DLL
  # as it will be "open" while R is using it
  on.exit(file.remove(path_src), add = TRUE)

  # Very important for `make` on Windows to swap out the winslashes with
  # `/` not `\\`, otherwise the SHLIB call will not work
  path_src <- normalizePath(path_src, winslash = "/", mustWork = FALSE)
  path_so <- normalizePath(path_so, winslash = "/", mustWork = FALSE)

  write_lines(path_src, lines)

  output <- r_shlib(path_src, path_so)

  if (show) {
    cat(output, sep = "\n\n")
    cat("\n")
  }

  dll_info <- dyn.load(path_so)

  out <- map(info, function(x) make_function(x$name_export, x$args, dll_info))
  names(out) <- map_chr(info, function(x) x$name_export)

  out
}

# ------------------------------------------------------------------------------

# Thanks Hadley :P
make_formals <- function(args) {
  n_args <- length(args)

  missing_arg <- list(quote(expr = ))
  fn_args <- rep(missing_arg, n_args)

  names(fn_args) <- args
  fn_args <- as.pairlist(fn_args)

  fn_args
}

make_body <- function(args, pointer) {
  n_args <- length(args)
  idx <- c(1L, 2L, rep(3L, n_args))

  body <- quote(CALL(NAME, ARG))
  body <- body[idx]

  body[[1L]] <- quote(.Call)
  body[[2L]] <- pointer

  for (i in seq_len(n_args)) {
    body[[i + 2L]] <- as.symbol(args[[i]])
  }

  body
}

get_pointer_to_symbol <- function(symbol, dll_info) {
  getNativeSymbolInfo(symbol, dll_info)$address
}

make_function <- function(name, args, dll_info) {
  symbol <- paste0("cbuild_", name)

  fn <- function() {}

  pointer <- get_pointer_to_symbol(symbol, dll_info)

  formals(fn) <- make_formals(args)
  body(fn) <- make_body(args, pointer)

  fn
}

add_default_includes <- function(lines) {
  c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    lines
  )
}

add_provided_includes <- function(lines, includes) {
  if (!is.character(includes)) {
    abort("`includes` must be a character vector, or `NULL`.")
  }

  if (length(includes) == 0L) {
    abort("At least one `includes` must be provided if `includes` is not `NULL`.")
  }

  is_header <- grepl_fixed(includes, ".h")

  if (any(!is_header)) {
    abort("`includes` must all be header files ending in `.h`.")
  }

  has_include <- grepl_fixed(includes, "#include")

  if (any(has_include)) {
    abort("`includes` should not contain `#include`, cbuild will add it for you.")
  }

  has_angles <- grepl_fixed(includes, "<") | grepl_fixed(includes, ">")

  if (any(has_angles)) {
    abort("`includes` should not contain angled brackets (`<` or `>`), cbuild will add it for you")
  }

  includes <- paste0("#include <", includes, ">")

  c(
    includes,
    lines
  )
}

add_no_remap <- function(lines) {
  c(
    "#define R_NO_REMAP",
    lines
  )
}

r_shlib <- function(path_src, path_so) {
  r_path <- paste0(
    R.home(component = "bin"),
    .Platform$file.sep,
    "R"
  )

  arg_src <- path_src

  arg_so <- paste(
    "-o",
    path_so
  )

  cmd <- paste(
    r_path,
    "CMD SHLIB",
    arg_src,
    arg_so
  )

  system(cmd, intern = TRUE)
}
