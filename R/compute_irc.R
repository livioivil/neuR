#' @name compute.irc
#' @title Computes IRC (intra run correlation) of a neuR-object
#'
#' @description Computes IRC (intra run correlation) of a neuR-object pcs array of slot data of a neuR-object (i.e. D@data$pcs)
#' @param D a neuR-ogject
#' @param pc.num 1  
#' @return a 3D array
#' @export

compute.irc <- function(D,pc.num=1){
  # estrae la percentuale di var spiegata dalla prima componente (su una singola decomposizione, singolo voxel)
   irc=D@data$var[,,pc.num,drop=FALSE]/D@data$var.tot
   irc
}
