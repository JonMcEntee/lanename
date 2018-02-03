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
convert <- function(list1, list2) {
  n <- length(list1)
  m <- length(list2)
  if (n != m) stop('list1 and list2 must be of same length')
  function(x) {
    if (!(x %in% list1)) return(NA)
    list2[which(list1 == x)]
  }
}
