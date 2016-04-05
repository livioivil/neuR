# calcola i valori T a partire dalla mappa di P e dal
P2T <- function(D,tail=0,
                PMap="spmP",
                SignMap="spmT"){
  if(is.null(D$info$df.res.unico)) D$info$df.res.unico=Inf
  if(tail==0){  
    spmT= -sign(.get_volume(D,SignMap))*qt(.get_volume(D,PMap)/2,
                                          D$info$df.res.unico)
  } else {
    spmT=-tail*qt(.get_volume(D,PMap),
                                     D$info$df.res.unico)
  }
  spmT
}