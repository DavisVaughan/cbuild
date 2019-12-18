write_api <- function(path, pkg, info, debug) {
  if (!has_public_callables(info)) {
    return(new_api_result())
  }

  if (!can_write_api(path, pkg)) {
    return(new_api_result())
  }

  lines_api_c <- write_api_c(pkg, info)
  lines_api_h <- write_api_h(pkg, info)

  out <- new_api_result(lines_api_c, lines_api_h)

  if (debug) {
    return(out)
  }

  if (!has_inst(path)) {
    create_inst(path)
  }

  if (!has_include(path)) {
    create_include(path)
  }

  path_api_c <- path_api_c(path, pkg)
  path_api_h <- path_api_h(path, pkg)

  remove_preexisting(path_api_c)
  remove_preexisting(path_api_h)

  create_new(path_api_c)
  create_new(path_api_h)

  write_lines(path_api_c, lines_api_c)
  write_lines(path_api_h, lines_api_h)

  out
}

# ------------------------------------------------------------------------------

new_api_result <- function(api_c = character(), api_h = character()) {
  list(api_c = api_c, api_h = api_h)
}

# ------------------------------------------------------------------------------

write_api_c <- function(pkg, info) {
  lines <- character()

  lines <- write_do_not_modify(lines)
  lines <- write_include_api_h(lines, pkg)
  lines <- write_global_declarations(lines, info)
  lines <- write_init_api(lines, pkg, info)

  lines
}

write_include_api_h <- function(lines, pkg) {
  pkg_h <- double_quote(paste0(pkg, ".h"))
  include <- paste0("#include ", pkg_h)

  c(lines, include, new_line())
}

write_init_api <- function(lines, pkg, info) {
  header <- paste0("void ", pkg, "_init_api() {")
  footer <- paste0("}")

  info <- slice_public_callables(info)

  signatures <- info$signature

  return <- map_chr(signatures, function(x) x$return)

  types <- map(signatures, function(x) x$arg_types)
  types <- map(types, function(x) paste0(x, collapse = ", "))

  names <- map(signatures, function(x) x$name_callable)

  define_globals <- paste0(
    "  ",
    names,
    " = (",
    return,
    " (*)(",
    types,
    ")) ",
    "R_GetCCallable(",
    double_quote(pkg),
    ", ",
    double_quote(names),
    ");"
  )

  lines <- c(lines, new_line())
  lines <- add_lines(lines, header)
  lines <- add_lines(lines, define_globals)
  lines <- add_lines(lines, footer)

  lines
}

write_global_declarations <- function(lines, info) {
  base <- make_callable_declaration_base(info)
  declarations <- paste0(base, " = NULL;")

  lines <- c(lines, new_line())
  lines <- add_lines(lines, declarations)

  lines
}

make_callable_declaration_base <- function(info) {
  info <- slice_public_callables(info)

  signatures <- info$signature

  return <- map_chr(signatures, function(x) x$return)

  types <- map(signatures, function(x) x$arg_types)
  types <- map(types, function(x) paste0(x, collapse = ", "))

  names <- map(signatures, function(x) x$name_callable)

  base <- paste0(return, " (*", names, ")(", types, ")")

  base
}

# ------------------------------------------------------------------------------

write_api_h <- function(pkg, info) {
  lines <- character()

  lines <- write_do_not_modify(lines)
  lines <- write_header_guard_open(lines, pkg)
  lines <- write_api_includes(lines)
  lines <- write_global_definitions(lines, info)
  lines <- write_init_api_definition(lines, pkg)
  lines <- write_header_guard_close(lines)

  lines
}

write_header_guard_open <- function(lines, pkg) {
  pkg_h <- paste0(toupper(pkg), "_H")

  ifndef <- paste0("#ifndef ", pkg_h)
  define <- paste0("#define ", pkg_h)

  lines <- add_lines(lines, ifndef)
  lines <- add_lines(lines, define)

  lines
}

write_api_includes <- function(lines) {
  c(
    lines,
    new_line(),
    "#include <Rinternals.h>",
    new_line(),
    "#include <R_ext/Rdynload.h>",
    new_line(),
    "#include <stdbool.h>",
    new_line()
  )
}

write_global_definitions <- function(lines, info) {
  base <- make_callable_declaration_base(info)
  definitions <- paste0(base, ";")

  lines <- c(lines, new_line())
  lines <- add_lines(lines, definitions)

  lines
}

write_init_api_definition <- function(lines, pkg) {
  init_api_definition <- paste0("void ", pkg, "_init_api();")

  lines <- c(lines, new_line())
  lines <- add_lines(lines, init_api_definition)

  lines
}

write_header_guard_close <- function(lines) {
  endif <- "#endif"

  lines <- c(lines, new_line())
  lines <- add_lines(lines, endif)

  lines
}

# ------------------------------------------------------------------------------

has_public_callables <- function(info) {
  info <- info[info$attribute == "callable",]

  if (nrow(info) == 0L) {
    return(FALSE)
  }

  is_hidden <- map_lgl(info$args, function(x) x$hidden)
  info <- info[!is_hidden,]

  if (nrow(info) == 0L) {
    return(FALSE)
  }

  TRUE
}

slice_public_callables <- function(info) {
  info <- info[info$attribute == "callable",]
  is_hidden <- map_lgl(info$args, function(x) x$hidden)
  info <- info[!is_hidden,]
  info
}

create_new <- function(path) {
  success <- file.create(path)

  if (!success) {
    abort("The creation of the fresh `", path, "` file was not successful.")
  }

  invisible()
}

remove_preexisting <- function(path) {
  if (!file.exists(path)) {
    return(invisible())
  }

  success <- file.remove(path)

  if (!success) {
    abort("The removal of the `", path, "` file was not successful.")
  }

  invisible()
}

can_write_api <- function(path, pkg) {
  if (!has_include(path)) {
    return(TRUE)
  }

  has_api_c <- has_path_api_c(path)
  has_api_h <- has_path_api_h(path)

  if (!has_api_c && !has_api_h) {
    return(TRUE)
  }

  first_line_api_c <- readLines(path_api_c(path), n = 1L)
  first_line_api_h <- readLines(path_api_h(path), n = 1L)

  made_by_cbuild <-
    identical(first_line_api_c, do_not_modify()) &&
    identical(first_line_api_h, do_not_modify())

  if (made_by_cbuild) {
    return(TRUE)
  }

  api_c <- paste0("`inst/include/", pkg, ".c`")
  api_h <- paste0("`inst/include/", pkg, ".h`")

  cat_line(paste0(
    api_c, " and ", api_h, " ",
    "files exist, but at least one was not made by cbuild. ",
    "Is it okay to overwrite them?"
  ))

  answer <- utils::menu(c("Yes", "No"))

  if (answer == 0L || answer == 2L) {
    FALSE
  } else {
    TRUE
  }
}

create_inst <- function(path) {
  ok <- dir.create(dir_inst(path))

  if (!ok) {
    abort("Could not create the `inst/` directory.")
  }

  invisible(path)
}

create_include <- function(path) {
  ok <- dir.create(dir_include(path))

  if (!ok) {
    abort("Could not create the `inst/include/` directory.")
  }

  invisible(path)
}

has_path_api_c <- function(path, pkg) {
  file.exists(path_api_c(path, pkg))
}

has_path_api_h <- function(path, pkg) {
  file.exists(path_api_h(path, pkg))
}

path_api_c <- function(path, pkg) {
  file.path(path, paste0(pkg, ".c"))
}

path_api_h <- function(path, pkg) {
  file.path(path, paste0(pkg, ".h"))
}

dir_inst <- function(path) {
  file.path(path, "inst")
}

dir_include <- function(path) {
  file.path(dir_inst(path), "include")
}

has_inst <- function(path) {
  dir.exists(dir_inst(path))
}

has_include <- function(path) {
  dir.exists(dir_include(path))
}
