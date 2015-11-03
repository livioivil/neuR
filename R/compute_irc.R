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
