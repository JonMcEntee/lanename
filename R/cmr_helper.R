#' Produces a string that describes the a lane
#'
#' @param days_back the days_back parameter of the lane
#' @param rate_type the rate_type parameter of the lane
#' @param equip_type the equipment_category of the lane
#' @param h the number of time units forecasted out
#' @param density the time unit of the forecast (for example 'day' or 'week')
#' @return a string that describes the use case in human readable language
#' @export
readable_lane <- function(days_back, rate_type, equip_type = NULL, h = NULL, density = NULL) {
  if (is.na(days_back) && is.na(rate_type) && is.na(equip_type)) {
    stop('one of the parameters: days_back, rate_type, or equip_type must be defined')
  }

  days <- if (!is.null(days_back)) paste0(days_back, '-Day') else NULL
  type <- if (!is.null(rate_type)) rate_type else NULL
  equip <- if (!is.null(equip_type)) paste0('for ', equip_type, 's') else NULL
  days_out <- if(!is.null(h) && !is.null(density)) paste0('(', h, ' ', density, 's out)')

  return(paste(c(days, type, "Rates", equip, days_out), collapse = ' '))
}

