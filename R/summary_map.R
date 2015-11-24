#' @name summary.map
#' @title summary of all maps
#' @description summary of all maps
#' @param object a neuR-object
#' @param maps NULL (default) or a vecotr of names of maps
#' @return a table of summary stats for each volume
#' @export

summary.map <- function(object,maps=NULL){
  if(is.null(maps)) maps=names(object)
   lapply(maps, function(map){
      out=apply(object@data[[map]],c(1,3), function(x)
        c(summary(x), 
       NA.proportion= mean(is.na(x)),
       sd=sd(x,na.rm = TRUE)))
#       colnames(out)=names.map(object)[[2]]
      out
      })
}