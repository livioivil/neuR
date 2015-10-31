compute.irc <- function(D,pc.num=1){
  # estrae la percentuale di var spiegata dalla prima componente (su una singola decomposizione, singolo voxel)
   irc=D@data$var[,,pc.num]/D@data$var.tot
   irc
}
