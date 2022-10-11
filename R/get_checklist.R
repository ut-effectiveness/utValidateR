

#' Returns the checklist for a given file and type
#'
#' @param file e.g. "student"
#' @param type either "database" or "ushe"
#' @export
get_checklist <- function(file = c("student", "student course",
                                   "graduation", "course",
                                   "buildings", "rooms"),
                          type = c("database", "ushe")) {

  file <- match.arg(file, several.ok = TRUE) # with several.ok, default is to include everything
  type <- match.arg(type, several.ok = TRUE)

  # full checklist from package data
  data("checklist", package = "utValidateR", envir = environment())

  # namespace hygiene
  file_in <- file
  type_in <- type

  out <- checklist %>%
    dplyr::filter(tolower(.data$file) %in% file_in,
                  tolower(.data$type) %in% type_in)
  out
}
