#' A `cbuild` engine for `knitr`
#'
#' @description
#' This provides a `cbuild` engine for `knitr`, which is used for rendering
#' C chunks. It is automatically registered when cbuild is loaded with
#' `library(cbuild)`, so there is no need to set it manually.
#'
#' To use it, create a standard R Markdown chunk, and put `cbuild` as the
#' engine name, like this:
#'
#' ``````
#' ```{cbuild}
#' // [[ export ]]
#' SEXP fn(SEXP x) {
#'   return x;
#' }
#' ```
#' ``````
#'
#' The engine is powered by [source_code()].
#'
#' By default, the engine will assign any functions that have been marked
#' with `// [[ export ]]` into the global environment, but this can be
#' controlled with the knitr option, `cbuild.env`, see below.
#'
#' @details
#' `eng_cbuild()` is an alternative to the default C chunk renderer, which
#' loads the dynamic library but does not bind the C pointers to any R
#' functions.
#'
#' @section Knitr Options:
#'
#' There are a number of knitr options that can be set for the cbuild engine.
#'
#' - `cbuild.includes`: Passed on to the `includes` option of `source_code()`.
#'
#' - `cbuild.no_remap`: Passed on to the `no_remap` option of `source_code()`.
#'
#' - `cbuild.env`: A single character string specifying the name of a
#'   pre-existing environment object to create the functions in. If `cbuild.env`
#'   is left unset, the default is to assign in the global environment.
#'
#' This is an example of how you could customize all 3 options:
#'
#' First create an R chunk with an alternative environment:
#'
#' ``````
#' ```{r}
#' env <- new.env()
#' ```
#' ``````
#'
#' Now create our C chunk:
#'
#' ``````
#' ```{cbuild, cbuild.includes = "Rdefines.h", cbuild.env = env, cbuild.no_remap = FALSE}
#' // [[ export ]]
#' SEXP fn(SEXP x) {
#'   // `NUMERIC_POINTER()` is only available in `Rdefines.h`
#'   double* p_x = NUMERIC_POINTER(x);
#'
#'   // Would be `Rf_ScalarReal()` if no remap was done
#'   return ScalarReal(p_x[0]);
#' }
#' ```
#' ``````
#'
#' After running the C chunk, we can do:
#'
#' ``````
#' ```{r}
#' env$fn(2)
#' # [1] 2
#' ```
#' ``````
#'
#' @export
eng_cbuild <- function (options) {
  if (!is_installed("knitr")) {
    abort("`knitr` must be installed to use the engine")
  }

  includes <- options$cbuild.includes
  no_remap <- options$cbuild.no_remap
  env <- options$cbuild.env

  if (is.null(no_remap)) {
    no_remap <- TRUE
  }

  if (is.null(env)) {
    env <- globalenv()
  } else {
    env <- get(env, envir = knitr::knit_global())
  }

  fns <- source_code(options$code, includes = includes, no_remap = no_remap)
  fn_names <- names(fns)

  for(i in seq_along(fns)) {
    fn <- fns[[i]]
    fn_name <- fn_names[[i]]

    assign(fn_name, fn, envir = env)
  }

  knitr::engine_output(options = options, code = options$code, out = character())
}

is_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
