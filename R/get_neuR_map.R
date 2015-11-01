#' @name get.neuR.map
#' @title add funct to neuR-object
#' @description NOT IMPLEMENTED YET: out.funct.name=map the name of the function to be generated (eg different names depending on the parameters)
#' @param obj neuR-object
#' @param map  function to be returned
#' @param recompute FALSE
#' @return a volume
#' @export

get.neuR.map <- function(obj,map,recompute=FALSE,...){
  if(!recompute){
    if(map%in%names(obj@data))
    return(obj@data[[map]])
  }
  #
  if(!(map%in%names(obj@functs))){
    obj@functs[[map]]= get.neuR.funct(map,...)
  }
  return(obj@functs[[map]](obj,...))
}