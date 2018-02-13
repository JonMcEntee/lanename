#' Produces a string that describes the use case of a time series
#'
#' @param days_back the days_back parameter of the lane
#' @param rate_type the rate_type parameter of the lane
#' @param equip_type the equipment_category of the lane
#' @return a string that describes the use case in human readable language
#' @export
readable_use_case <- function(days_back = NULL, rate_type = NULL, equip_type = NULL) {
  if (is.null(days_back) && is.null(rate_type) && is.null(equip_type)) {
    stop('one of the parameters: days_back, rate_type, or equip_type must be defined')
  }

  days <- if (!is.null(days_back)) paste0(days_back, '-Day') else NULL
  type <- if (!is.null(rate_type)) rate_type else NULL
  equip <- if (!is.null(equip_type)) paste0('for ', equip_type, 's') else NULL

  return(paste(c(days, type, "Rates", equip), collapse = ' '))
}