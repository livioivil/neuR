#' @name add.neuR.map
#' @title add volume to neuR-object
#' @description add volume to neuR-object
#' @param obj neuR-object
#' @param map  name of the map (function) to be added. It can also be a list of maps (i.e. 3D arrays)
#' @param out.name equal to map (or names(map) if map is a list) by default. 
#' Name of the map to be added to the returned object.
#' @return a neuR-object
#' @export


add.neuR.map <- function(obj,map,out.name=NULL,...){
  if (is.list(map)){
    if(is.null(out.name)){ 
      out.name=names(map)
    }
  } else {
    if(is.null(out.name)){ 
      out.name=map
    }
    map=get.neuR.map (obj,map,...)
  }
  
  map= .check.map(obj,map)
  if(is.list(map)) {
    obj@data[out.name]=map 
    } else  {
      obj@data[[out.name]]=map
    }
  obj
}