#' @name add.neuR.funct
#' @title add funct to neuR-object
#' @description add funct to neuR-object
#' @param obj neuR-object
#' @param map  function to be returned
#' @param out.name map by default. name of the function to be added in obj@function$out.name
#' @return a neuR-object
#' @export


add.neuR.funct <- function(obj,funct,out.name=funct,...){
  obj@functs[[out.name]]=get.neuR.funct(funct,...)
  obj
}