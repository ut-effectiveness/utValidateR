
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

    tryCatch({
      out2 <- eval_tidy(expr, df)

      assert_no_missing(out2, expr_chr)
      assert_logical(out2, expr_chr)
      assert_length(out2, df, expr_chr)
      out2
    },

    error = function(cond) {
      message("Check attempt failed for ", expr_chr, "\n",
              "With the following error:\n",
              cond, "\n\n")
      return(rep(FALSE, nrow(df)))
    })

  }

  outfun
}
