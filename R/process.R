#' Process package attributes
#'
#' @description
#' `process_attributes()` processes C level comment attributes. It automates a
#' number of tasks, such as:
#'
#' - Generation of a `src/init.c` file. This includes:
#'
#'   - Exporting C functions to the R side through the `.Call` and `.External`
#'     mechanisms.
#'
#'   - Registering C callables for use by other packages.
#'
#' - Optional generation of a pair of package API files in
#'   `inst/include/<pkg.c>` and `inst/include/<pkg.h>` for the public callables
#'   registered in the `init.c` file.
#'
#' `process_attributes()` determines the functions to include in the
#' `init.c` file through the use of C comments placed directly above the
#' function of interest and formatted like `// [[ export() ]]`. See the
#' sections below for a complete description.
#'
#' @param path `[character(1)]`
#'
#'   The relative file path to the top level of your package.
#'
#' @param debug `[logical(1)]`
#'
#'   Should the lines that will be used to construct the `init.c` and API files
#'   be returned as a named list of character vectors rather than written to
#'   disk?
#'
#' @return
#' A named list of 3 elements: `init`, `api_c`, and `api_h`. These contain the
#' lines written to their corresponding file. If `debug = FALSE`, this is
#' returned invisibly. If `debug = TRUE`, it is returned visibly and the
#' lines are not written to file.
#'
#' @section Export:
#'
#' Export a C function to the R side as a `CallRoutine`, suitable for use with
#' `.Call()`.
#'
#' ```
#' // [[ export(name = NA_character_) ]]
#' ```
#'
#' - `name`: `[character(1)]`
#'
#'   A character string with no spaces in the name. Used to override
#'   the name that is generated for the R routine object. The default uses
#'   the name of the exported C function as the name for the R routine.
#'
#' @section Export External / External2:
#'
#' Export a C function to the R side as an `ExternalRoutine`, suitable for use
#' with `.External()` or `.External2()`.
#'
#' ```
#' // [[ export_external(n, name = NA_character_) ]]
#' // [[ export_external2(n, name = NA_character_) ]]
#' ```
#'
#' - `n`: `[integer(1)]`
#'
#'   The number of arguments expected when calling this routine _from the
#'   R side_. Meaning that if you have a routine called `pkg_my_fun` that
#'   you plan to call like `.External(pkg_my_fn, arg1, arg2)`, then you should
#'   pass `n = 2`.
#'
#' - `name`: `[character(1)]`
#'
#'   A character string with no spaces in the name. Used to override
#'   the name that is generated for the R routine object. The default uses
#'   the name of the exported C function as the name for the R routine.
#'
#' @section Callable:
#'
#' Register a C function to be callable by other R packages.
#'
#' ```
#' // [[ callable(name = NA_character_, hidden = FALSE) ]]
#' ```
#'
#' - `name`: `[character(1)]`
#'
#'   A character string with no spaces in the name. Used to override
#'   the name that is generated for the callable object. The default uses
#'   the name of the exported C function as the name for the callable.
#'
#' - `hidden`: `[logical(1)]`
#'
#'   A logical. Should the registered callable also get an entry in the API
#'   files created in `./inst/include/`? The default includes it in the API.
#'   Flipping to `hidden = TRUE` registers the callable with
#'   `R_RegisterCCallable()` but does not generate an API entry for it, meaning
#'   that it can only be retrieved by another package's C code by using
#'   `R_GetCCallable()`.
#'
#' @section Init:
#'
#' Sometimes you need to initialize extra objects at package load time. By
#' marking a function with `init()`, it will get included at the end of the
#' call to `R_init_<pkg>()`, which is called whenever the package is loaded.
#' The function marked with `init()` should return `void` and take 1 argument,
#' a `DllInfo*`, typically given the variable name `dll`.
#'
#' ```
#' // [[ init() ]]
#' ```
#'
#' @export
process_attributes <- function(path = ".", debug = FALSE) {
  if (length(path) != 1L || !is.character(path) || is.na(path)) {
    abort("`path` must be a string.")
  }

  if (length(debug) != 1L || !is.logical(debug) || is.na(debug)) {
    abort("`debug` must be a bool (TRUE / FALSE).")
  }

  if (!dir.exists(path)) {
    abort("`path` must point to an existing folder containing an R package.")
  }

  if (!has_src(path)) {
    abort("`path` must point to an R package with a `src` folder to process.")
  }

  pkg <- package_name(path)

  info <- collect_attributes_and_signatures(path)

  init <- write_init(path, pkg, info, debug)
  api <- write_api(path, pkg, info, debug)

  out <- c(init, api)

  if (debug) {
    return(out)
  }

  invisible(out)
}
