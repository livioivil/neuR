#' @name names.map
#' @title dimnames of the maps
#' @description dimnames of the maps
#' @param object a neuR-object
#' @return a list of dimnames
#' @export names.map
names.map <- function(object)
  object@info$dimnames