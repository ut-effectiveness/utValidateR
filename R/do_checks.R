
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

  # make_checker() returns a function (of a dataframe)
  checkfuns <- map2(checklist$rule, checklist$checker, ~make_checker(.x, .y))

  # Append checkfuns output to df_tocheck
  check_results <- map(checkfuns, ~.(df_tocheck)) %>%
    setNames(result_names) %>%
    bind_cols()

  # dataframe for whether each check induces a warning or failure
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


#' Returns number of days since each date in x
#'
#' @param x a Date vector or similar
get_age <- function(x) {
  lubridate::interval(x, Sys.Date()) %/% lubridate::days(x=1)
}

#' Returns a dataframe with columns (rule)_activity_date, (rule)_error_age
#'
#' @param df the dataframe to check
#' @param datecol Name of the relevant activity_date column
#' @param rule Name of the rule, e.g "S03b"
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom stats setNames
#' @export
get_activity_dates <- function(df, datecol, rule) {
  if (is.na(datecol)) return(NULL) # Some rules have no relevant date
  if (!exists(datecol, where = df)) {
    warning(sprintf("Could not find column %s", datecol))
    return(NULL)
  }

  out <- tibble(activity_date = df[[datecol]]) %>%
    mutate(error_age = get_age(.data$activity_date)) %>%
    setNames(paste(rule, names(.), sep = "_"))
  out
}
