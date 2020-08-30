#' @title netatmo_indoor object R6
#'
#' @param dataset dataset indoor
#'
#' @import R6
#'
#' @export
#'
#' @examples
#' dir_csv <- "f:/r/netatmo/csv"
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("indoor", "outdoor"))
#' db_lst <- load_csv(file_lst)
#' boo <- netatmo_indoor_obj$new(db_lst$db_netatmo_indoor)
netatmo_indoor <- R6::R6Class(
  "netatmo_indoor",

  # inherit ----
  inherit = default_data_frame,

  # public ----
  public = list(
    initialize = function(dataset) {
      stopifnot(
        colnames(dataset) == c(
          "location",
          "datetime",
          "date",
          "minute",
          "hour",
          "day",
          "week",
          "month",
          "month_abbr",
          "year",
          "temperature",
          "humidity",
          "c02",
          "noise",
          "pressure"
        )
      )
      dataset <- unique(data.frame(dataset))
      private$.data <- dataset
    },

    # finalize ----
    finalize = function() {
    }

  ),
  # private ----
  private = list(),

  # active ----
  active = list()
)


#' summary of netatmo_indoor object
#'
#' @param obj R6 object
#' @param ...
#'
#' @import dplyr
#' @import tidyr
#' @import knitr
#'
#' @return
#' @export
#'
#' @examples
netatmo_indoor$summary <- function(obj = NULL, ...) {
  stopifnot(!is.null(obj))
browser()
  boo <-
    dplyr::as_data_frame(obj$.data) %>%
    dplyr::select(location, year, month_abbr, temperature, humidity, c02, noise, pressure) %>%
    tidyr::gather(measure, value, -location, -year, -month_abbr) %>%
    dplyr::group_by(location, year, month_abbr, measure) %>%
    dplyr::summarise_all(funs(
      min = round(min(., na.rm = TRUE), 1),
      max = round(max(., na.rm = TRUE), 1),
      Q25 = round(quantile(., probs = 0.25, na.rm = TRUE), 1),
      Q75 = round(quantile(., probs = 0.75, na.rm = TRUE), 1),
      avg = round(mean(., na.rm = TRUE), 1),
      med = round(median(., na.rm = TRUE), 1),
      sum = round(sum(., na.rm = TRUE), 1),
      range = round(max - min, 1),
      n_obs = dplyr::n()
    )) %>%
    dplyr::arrange(location, year, month_abbr, measure)

  knitr::kable(head(boo, ...))
}

