# calcola i p-value a partire dalla mappa di T
T2P <- function(D,tail=0,
                Tvolume.name="spmT"){
  if(tail==0){ 
    spmP=2*pt(-abs(D@data[[Tvolume.name]]),D@info$df$df.res.unico)
  } else {
    spmP=pt(-tail*D@data[[Tvolume.name]],D@info$df$df.res.unico)
  }
  spmP
}
