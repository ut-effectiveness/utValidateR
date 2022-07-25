
#' Function factory for generating checker functions
#'
#' Result is a function with a single dataframe argument
#'
#' @param rule Name of the rule being applied, e.g. "S03b"
#' @param expr Expression that generates a logical vector when applied to a dataframe
#' @importFrom rlang eval_tidy enexpr
#' @export
make_checker <- function(rule, expr) {
  # expr <- enexpr(expr)
  expr_chr <- paste(deparse(expr), collapse = "\n")

  outfun <- function(df) {

    tryCatch({
      out2 <- eval_tidy(expr, df)

      assert_no_missing(out2)
      assert_logical(out2)
      assert_length(out2, df)
      out2
    },

    error = function(cond) {
      message("Check attempt failed for rule ", rule, ":\n",
              expr_chr, "\n",
              "With the following error:\n",
              cond, "\n")
      return(rep(FALSE, nrow(df)))
    })

  }

  outfun
}
