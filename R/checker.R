
#' Function factory for generating checker functions
#'
#' Result is a function with a single dataframe argument
#'
#' @param expr Expression that generates a logical vector when applied to a dataframe
#' @importFrom rlang eval_tidy enexpr
#' @export
make_checker <- function(expr) {
  # expr <- enexpr(expr)
  expr_chr <- deparse(expr)

  outfun <- function(df) {
    out2 <- try(eval_tidy(expr, df), silent = TRUE)
    if (inherits(out2, "try-error")) {
      # TODO: this could be simpler and more general with a tryCatch
      warning("Check attempt failed for ", expr_chr, "\n",
              "With the following error:\n",
              get_tryerror(out2), "\n\n")

      return(rep(FALSE, nrow(df)))
    }

    assert_no_missing(out2, expr_chr)
    assert_logical(out2, expr_chr)
    assert_length(out2, df, expr_chr)

    out2
  }
  outfun
}

#' Returns the text of the error message in a `try()` failure
#'
#' @param tryerror result of `try()` failure
get_tryerror <- function(tryerror) {
  cond <- attr(tryerror, "condition")
  cond$message
}
