
#' Show comparison between expected and obtained rule output, along with helpful info
#'
#' @param rule character name of rule, e.g. "S17b"
#' @param testdf dataframe with test data, like that found in extdata/student_unit_test.csv
#' @param mismatch_only if TRUE (default), only show rows where expected rule outcome does not match actual
#' @export
compare_rule_output <- function(rule, testdf = get_test_data("student"), mismatch_only = TRUE) {

  # Required data objects from utValidateR package
  data("checklist", package = "utValidateR", envir = environment())
  data("aux_info", package = "utValidateR", envir = environment())



  rule_in <- rule # namespace hygiene
  checklist_row <- checklist[checklist$rule == rule_in, ]
  stopifnot(nrow(checklist_row) == 1L)

  rule_expr <- checklist_row$checker[[1]]

  rule_checker <- make_checker(rule, expr = rule_expr,
                               env = new_environment(aux_info, parent = caller_env()))
  testdf_filt <- testdf %>%
    rename(expected = `Expected value`) %>%
    mutate(expr = paste(deparse(rule_expr), collapse = " ")) %>%
    filter(`USHE element` == "all" | `USHE rule` == rule_in) %>%
    mutate(rule = rule_in) %>%
    select(row_number, rule, expr,
           any_of(all.vars(rule_expr)),
           expected)

  testdf_filt$actual <- ifelse(rule_checker(testdf_filt), "pass", "fail")

  if (mismatch_only) {
    testdf_filt <- dplyr::filter(testdf_filt, actual != expected)
  }

  testdf_filt
}
