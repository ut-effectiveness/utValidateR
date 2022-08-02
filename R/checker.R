
#' Function factory for generating checker functions
#'
#' Result is a function with a single dataframe argument
#'
#' @param rule Name of the rule being applied, e.g. "S03b"
#' @param expr Expression that generates a logical vector when applied to a dataframe
#' @param env environment for evaluating checkers, passed to `rlang::eval_tidy()`
#' @importFrom rlang eval_tidy enexpr
#' @export
make_checker <- function(rule, expr, env = rlang::caller_env()) {
  # expr <- enexpr(expr)
  expr_chr <- paste(deparse(expr), collapse = "\n")

  # Function to return
  #
  # Returns a logical vector with length equal to nrow(df).
  # TRUE if passed, FALSE if failed
  #
  # @param df the dataframe to check
  outfun <- function(df) {

    tryCatch({
      out2 <- eval_tidy(expr, data = df, env = env)

      assert_no_missing(out2)
      assert_logical(out2)
      assert_length(out2, df)
      out2
    },

    error = function(cond) {
      msg <- paste0("`", expr_chr, "`\n")
      msg2 <- paste0("Failed with the following message:\n",
                     cond$message, "\n")

      if (nchar(cond$message) > 0) {
        msg <- paste0(msg, msg2)
      }

      message(rule, ": ", msg, "\n\n")
      return(rep(msg, nrow(df)))
    })

  }

  outfun
}
