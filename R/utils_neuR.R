#' @name names.map
#' @title dimnames of the maps
#' @description dimnames of the maps
#' @param object a neuR-object
#' @return a list of dimnames
#' @export names.map
names.map <- function(object)
  object@info$dimnames

#' @title dim of a given map
#' @description dim of a given map
#' @param object a neuR-object
#' @param map.name the map to know the dims 
#' @return a vector of dims: n replicates X nvoxels X n dimensions
#' @export dim.map
dim.map <- function(object,map.name)
  dim(object@data[[map.name]])

#' @title Number of voxels of an neuR-object
#' @description Number of voxels of an neuR-object
#' @param object a neuR-object
#' @return Number of voxels of an neuR-object, i.e. a scalar
#' @export nvoxels
nvoxels <- function(object)
  object@info$nvoxels
