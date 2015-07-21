compute.irh <- function(pc,pc.num=1){
  # indice di omogeneità dei k loadings. indice in scala 0-1. 
  # la norma essendo pari a 1, implica un momento secondo pari a 1/k.
  # il momento secondo è decomponibile in varianza + media^2.
  # l'indice è pari a media(loadings)^2*k.
  # se 1 tutti i loading sono uguali tra loro (e pari a 1/sqrt(k))
  # se 0, i loadings hanno media 0 e la varianza vale 1/k
  
   irh=t(colMeans(D@data$loadings)^2*nrow(D@data$loadings))
   irh=array(irh,c(dim(irh)[1:2],1))
   rownames(irh)=dimnames(D@data$loadings)[[3]]
   colnames(irh)=colnames(D@data$loadings)
   irh
}
