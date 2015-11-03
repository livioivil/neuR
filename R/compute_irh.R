#' @name compute.irh
#' @title Computes IRH (intra run homogeneity) of a neuR-object
#'
#' @description Computes IRH (intra run homogeneity) of a neuR-object pcs array of slot data of a neuR-object (i.e. D@data$pcs)
#' indice di omogeneità dei k loadings. indice in scala 0-1. 
#' la norma essendo pari a 1, implica un momento secondo pari a 1/k.
#' il momento secondo è decomponibile in varianza + media^2.
#' l'indice è pari a media(loadings)^2*k.
#' se 1 tutti i loading sono uguali tra loro (e pari a 1/sqrt(k))
#' se 0, i loadings hanno media 0 e la varianza vale 1/k
#' @param D a neuR-ogject
#' @param pc.num 1  
#' @param drop.tcs FALSE
#' @return a 3D array
#' @export

compute.irh <- function(D,pc.num=1,drop.tcs=FALSE){
  # indice di omogeneità dei k loadings. indice in scala 0-1. 
  # la norma essendo pari a 1, implica un momento secondo pari a 1/k.
  # il momento secondo è decomponibile in varianza + media^2.
  # l'indice è pari a media(loadings)^2*k.
  # se 1 tutti i loading sono uguali tra loro (e pari a 1/sqrt(k))
  # se 0, i loadings hanno media 0 e la varianza vale 1/k
  out <- .compute.irh
  environment(out) <- sys.frame(sys.nframe())
  environment(out)$loadings=D@data$loadings
  
  D@data=c(D@data,irc=out())
  if(drop.tcs) D@data$tcs=NULL
  D
}

.compute.irh <- function(){
  irh=t(apply(loadings,c(2,3),mean)^2*nrow(loadings))
  irh=array(irh,c(dim(irh)[1:2],1))
  rownames(irh)=dimnames(loadings)[[3]]
  colnames(irh)=colnames(loadings)
  irh
}
