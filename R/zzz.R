# nocov start
.onLoad <- function(libname, pkgname) {
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

    assign("startsWith", startsWith, envir = asNamespace("cbuild"))
    assign("endsWith", endsWith, envir = asNamespace("cbuild"))
  }

  # Register knitr engine
  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::knit_engines$set(cbuild = asNamespace("cbuild")$eng_cbuild)
  }
}

# nocov end
