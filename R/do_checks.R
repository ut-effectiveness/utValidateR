
#' Apply all rules in checklist to a dataframe
#'
#' Returns df_tocheck with additional columns for each rule in checklist
#'
#' @param df_tocheck a data.frame to check, like fake_student_df
#' @param checklist a tibble containing rule functions in `checker` column, as
#'  well as `rule` and `activity_date` columns
#' @param aux_info list (or environment) with additional objects used by the checkers
#'
#' @importFrom purrr map map2 map2_dfc
#' @importFrom dplyr bind_cols all_of select
#' @importFrom rlang new_environment caller_env
#' @export
do_checks <- function(df_tocheck, checklist, aux_info) {

  # must be an environment to be passed to the env argument in rlang::eval_tidy()
  if (is.list(aux_info))
    aux_info <- new_environment(data = aux_info,
                                parent = caller_env()) # Is this the right parent? Does it matter?

  # Activity dates are not relevant for USHE checks
  checklist$activity_date[checklist$type == "USHE"] <- NA

  # For each row in checklist, apply the checker function to df_tocheck
  result_names <- paste0(checklist$rule, "_status")

  # make_checker() returns a function with a single `df` argument
  checkfuns <- map2(checklist$rule, checklist$checker,
                    ~make_checker(.x, .y, env = aux_info))

  # Append checkfuns output to df_tocheck
  check_results <- map(checkfuns, ~.(df_tocheck)) %>%
    setNames(result_names) %>%
    bind_cols()

  # Function to write error messages to result instead of pass/fail
  get_status_chr <- function(resultvec, status) {
    if (is.character(resultvec)) return(resultvec)
    ifelse(resultvec, "Pass", status)
  }

  # dataframe for whether each check induces a warning or failure
  check_statuses <- map2_dfc(check_results, checklist$status,
                             ~get_status_chr(.x, .y))

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
    warning(sprintf("Could not find column %s\n", datecol))
    return(NULL)
  }

  out <- tibble(activity_date = df[[datecol]]) %>%
    mutate(error_age = get_age(.data$activity_date)) %>%
    setNames(paste(rule, names(.), sep = "_"))
  out
}
