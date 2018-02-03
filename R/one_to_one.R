data("market_areas")

#' Converts KMA codes into human readable form
#'
#' For example, converts "TX_DAL" to "Dallas, TX"
#'
#' @param x a KMA code in string format to be converted
#' @return a human readable city name
#' @export
kma_to_readable <- convert(market_areas$kma, market_areas$full_name)
