#' @export
source_file <- function(file, includes = TRUE, remap = FALSE, show = FALSE) {
  file <- normalizePath(file, mustWork = TRUE)

  lines <- read_lines(file)

  info <- parse_exports(lines)

  lines <- replace_function_names(lines, info)

  if (includes) {
    lines <- add_default_includes(lines)
  }

  if (!remap) {
    lines <- add_no_remap(lines)
  }

  dir_tmp <- tempdir()
  dir_cbuild <- file.path(dir_tmp, "cbuild")

  dir.create(dir_cbuild)
  on.exit(unlink(dir_cbuild, recursive = TRUE, force = TRUE), add = TRUE)

  path_src <- tempfile("cbuild_", tmpdir = dir_cbuild, fileext = ".c")
  path_so <- tempfile("cbuild_", tmpdir = dir_cbuild, fileext = .Platform$dynlib.ext)

  write_lines(path_src, lines)

  output <- r_shlib(path_src, path_so)

  if (show) {
    cat(output, sep = "\n\n")
    cat("\n")
  }

  dll_info <- dyn.load(path_so)

  out <- map(info, function(x) make_function(x$name_symbol, x$args, dll_info))
  names(out) <- map_chr(info, function(x) x$name)

  out
}

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

make_function <- function(symbol, args, dll_info) {
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

  arg_src <- shQuote(path_src)

  arg_so <- paste(
    "-o",
    shQuote(path_so)
  )

  cmd <- paste(
    r_path,
    "CMD SHLIB",
    arg_src,
    arg_so
  )

  system(cmd, intern = TRUE)
}
