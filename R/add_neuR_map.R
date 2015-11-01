#' @name add.neuR.map
#' @title add volume to neuR-object
#' @description add volume to neuR-object
#' @param obj neuR-object
#' @param map  function to be returned
#' @param out.name map by default. name of the map to be added in obj@function$out.name
#' @return a neuR-object
#' @export


add.neuR.map <- function(obj,map,out.name=map,...){
  temp=get.neuR.map (obj,map,...)
  if(is.list(temp)) 
    obj@data[names(temp)]=temp else  
      obj@data[[out.name]]=temp
  obj
}