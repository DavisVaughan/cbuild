cbuild__hook_env <- NULL

# nocov start
.onLoad <- function(libname, pkgname) {
  cbuild__hook_env <<- new.env(parent = baseenv())
  ns <- asNamespace("cbuild")

  if (getRversion() < '3.3.0') {
    startsWith <- function (x, prefix){
      if (!is.character(x) || !is.character(prefix)) {
        stop("non-character object(s)")
      }
      suppressWarnings(substr(x, 1L, nchar(prefix)) == prefix)
    }

    endsWith <- function (x, suffix) {
      if (!is.character(x) || !is.character(suffix)) {
        stop("non-character object(s)")
      }
      n = nchar(x)
      suppressWarnings(substr(x, n - nchar(suffix) + 1L, n) == suffix)
    }

    assign("startsWith", startsWith, envir = ns)
    assign("endsWith", endsWith, envir = ns)
  }

  # Register knitr engine
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(cbuild = ns$eng_cbuild)
  }

  # Load hooks
  assign("export", ns$hook_export, envir = cbuild__hook_env)
  assign("export_external2", ns$hook_export_external2, envir = cbuild__hook_env)
  assign("export_external", ns$hook_export_external, envir = cbuild__hook_env)
  assign("callable", ns$hook_callable, envir = cbuild__hook_env)
  assign("init", ns$hook_init, envir = cbuild__hook_env)
}

# nocov end
