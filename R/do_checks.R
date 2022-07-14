
#' Apply all rules in checklist to a dataframe
#'
#' Returns df_tocheck with additional columns for each rule in checklist
#'
#' @param df_tocheck a data.frame to check, like fake_student_df
#' @param checklist a tibble containing rule functions in `checker` column, as
#'  well as `rule` and `activity_date` columns
#'
#' @importFrom purrr map map2 map2_dfc
#' @importFrom dplyr bind_cols all_of select
#' @export
do_checks <- function(df_tocheck, checklist) {
  # For each row in checklist, apply the checker function to df_tocheck
  result_names <- paste0(checklist$rule, "_status")

  checkfuns <- map(checklist$checker, ~make_checker(.))

  check_results <- map(checkfuns, ~.(df_tocheck)) %>%
    setNames(result_names) %>%
    bind_cols()

  check_statuses <- map2_dfc(check_results, checklist$status,
                             ~ifelse(.x, "Pass", .y))

  # dataframe with activity_date and error_age columns for relevant rules
  actdate_dfs <- map2_dfc(checklist$activity_date, checklist$rule,
                          ~get_activity_dates(df_tocheck, datecol = .x, rule = .y))

  if (!length(actdate_dfs)) actdate_dfs <- NULL # Otherwise bind_cols fails

  # result, date, and age, sorted by name
  # TODO: fix ordering. _status should be first, then _activity_date, then _error_age
  df_tojoin <- bind_cols(check_statuses, actdate_dfs) %>%
    select(all_of(sort(names(.))))

  out <- bind_cols(df_tocheck, df_tojoin)
  out
}
