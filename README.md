
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbuild

<!-- badges: start -->

[![R build
status](https://github.com/DavisVaughan/cbuild/workflows/R-CMD-check/badge.svg)](https://github.com/DavisVaughan/cbuild)
[![Travis build
status](https://travis-ci.org/DavisVaughan/cbuild.svg?branch=master)](https://travis-ci.org/DavisVaughan/cbuild)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/cbuild/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/cbuild?branch=master)
<!-- badges: end -->

The goal of cbuild is to provide tools for working with C both
interactively and when constructing an R package. The two broad goals
are:

  - Provide a way to interactively source C code into your R session.
    See `source_function()` and `source_code()` to get started.

  - Provide an automatic registration system for R package developers
    that use C, see `process_attributes()`. It can automatically
    generate the `init.c` file for you, using a comment system similar
    to `Rcpp::export`. For example, the following would generate an
    entry for the C function `fn()` in `init.c`, and generate the glue
    code to export it as an R routine named `fn`, which you could call
    from the R side with `.Call(fn, 1)`:
    
        // [[ export() ]]
        SEXP fn(SEXP x) {
          return x;
        }

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/cbuild")
```

## Examples

``` r
library(cbuild)
```

The easiest way to get started is with `source_function()`, which allows
you to source a C function from text. It automatically includes `R.h`
and `Rinternals.h` for you to use.

``` r
fn <- source_function("
  SEXP fn(SEXP x) {
    return x;
  }
")

fn(1)
#> [1] 1
```

From there, you can use `source_code()` to source larger chunks of code.
Tag functions that you want to export with `// [[ export() ]]`.

``` r
fns <- source_code("
  static SEXP helper(SEXP x) {
    return x;
  }
  
  // [[ export() ]]
  SEXP fn1(SEXP x) {
    return helper(x);
  }
  
  // [[ export() ]]
  SEXP fn2(SEXP x, SEXP y) {
    double result = REAL(x)[0] + REAL(y)[0];
    return Rf_ScalarReal(result);
  }
")

fns$fn1(1)
#> [1] 1

fns$fn2(1, 2)
#> [1] 3
```

If you have a full file to source, you can use `source_file()`.
