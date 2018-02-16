#' Generator of conversion functions
#'
#' \code{convert} turns a function to convert an item present in \code{list1} to
#' the same index position in \code{list2}. For example, if \code{list1} contains
#' ("New York", "Paris", "Tokyo") and \code{list2} contains ("United States", "France",
#' "Japan"), then the function produced will convert city names to country names.
#'
#' @param list1 A list of items which can be converted.
#' @param list2 A list of items to be converted two. Both \code{list1} and \code{list2} must
#'   be of the same length as \code{list1}.
#' @return A function which can convert objects in \code{list1} to objects in \code{list2}.
#' @export
convert <- function(list1, list2) {
  n <- length(list1)
  m <- length(list2)
  if (n != m) stop('list1 and list2 must be of same length')
  function(x) {
    if (!(x %in% list1)) return(NA)
    list2[which(list1 == x)]
  }
}

data("market_areas")

#' Converts KMA codes into human readable form
#'
#' For example, converts "TX_DAL" to "Dallas, TX"
#'
#' @param x a KMA code in string format to be converted
#' @return a human readable city name
#' @export
kma_to_readable <- convert(market_areas$kma, market_areas$full_name)

#' Takes in a KMA code and returns the parent region of that KMA
#'
#' For example, 'TX_DAL' becomes "South Central"
#'
#' @param x a KMA code in string format to be converted
#' @return the KMA code's parent region
#' @export
find_kma_parent <- convert(market_areas$kma, market_areas$parent)

#' Takes in a Region and returns a string vector of child KMAs
#'
#' For example, "California" returns "CA_FRS" "CA_LAX" "CA_ONT" "CA_SDI" "CA_SFR" "CA_STK" "OR_MED"
#'
#' @param x a Region in string format to be converted
#' @return a list of KMA codes in string format
#' @export
find_region_children <- convert(regions$region, regions$children_kmas)

# functions to assist in parsing data from DS_CSB_CALC_MARKET_RATES

#' Convert EQUIPMENT_CATEGORY string into human readable format
#'
#' @param x an equipment type string ('V', 'F', or 'R')
#' @return a human readable string representing that equipment type
#' @export
readable_equip <- convert(c('V','F','R'), c('Van', 'Flatbed', 'Reefer'))

#' Convert RATE_TYPE string into human readable format
#'
#' @param x a rate type in integer format (1 or 2)
#' @return a human readable string representing that rate type
#' @export
readable_rate_type <- convert(c(1L, 2L), c('Contract', 'Spot'))
