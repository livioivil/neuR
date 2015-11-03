#' @name compute.irc
#' @title Computes IRC (intra run correlation) of a neuR-object
#'
#' @description Computes IRC (intra run correlation) of a neuR-object pcs array of slot data of a neuR-object (i.e. D@data$pcs)
#' @param D a neuR-ogject
#' @param pc.num 1  
#' @param drop.tcs FALSE
#' @return a 3D array
#' @export

compute.irc <- function(D,pc.num=1,drop.tcs=FALSE){
     out <- .compute.irc
     environment(out) <- sys.frame(sys.nframe())
     environment(out)$var=D@data$var[,,pc.num,drop=FALSE]
     environment(out)$var.tot=D@data$var.tot
     D@data=c(D@data,irc=out())
     if(drop.tcs) D@data$tcs=NULL
     D
}

.compute.irc <- function(){
  # estrae la percentuale di var spiegata dalla prima componente (su una singola decomposizione, singolo voxel)
  irc=var/var.tot
  irc[is.na(irc)]=0
  list(irc)
}

.set.default.params.compute.irc <- function(out,...){
  dotss=pryr::dots(...)
  dotss=sapply(dotss,eval)
  
  if(is.null(dotss$var)&& sum(names(dotss)=="")>0)
    names(dotss)[which(names(dotss)=="")[1]]="var"
  
  if(is.null(dotss$pc.num))
    dotss$pc.num=1
  if(is(dotss$var,"neuR.object")){
    dotss$var=D@data$var[,,dotss$pc.num,drop=FALSE]
    dotss$var.tot=D@data$var.tot
  }
  if(is.null(dotss$var))
    dotss$var=D@data$var[,,dotss$pc.num,drop=FALSE]
  if(is.null(dotss$var.tot))
    dotss$var.tot=D@data$var.tot
  
  environment(out) <-list2env(dotss)# sys.frame(sys.nframe())
  out
}