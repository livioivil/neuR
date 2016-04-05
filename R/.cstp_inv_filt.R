.cstp_inv_filt<-function(list.eigen){
  
  col.filt=matrix(0,nrow=dim(list.eigen[[2]])[1],ncol=dim(list.eigen[[2]])[2])
  
  #cicli for che mi calcola le colonne del filtro
  for(i in 1:length(list.eigen[[1]])){
    col.filt[,i]=list.eigen[[1]][i]^(1/2)*list.eigen[[2]][,i]
    filt=col.filt
  }
  return(filt)
}