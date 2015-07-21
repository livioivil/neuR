write.volumes <- function(D,which.maps="tcs",file="V_"){
  #generica funzione che scrive i volumi di una matrice D time X voxel X block (oppue x pc)
  .write. <- function(D4,mask,file){
    if(is.null(mask)){ #allora ci deve essere almeno dim.vol
      D4=array(D4,c(nrow(D4),dim.vol,dim(D4)[3]))
      sapply(1:nrow(D4), function(i)
        sapply(1:dim(D4)[3], function(j){
          f.write.analyze(D4[i,,,,j],paste(file,i,sep=""),size="float")
        })      )
    } else {
      sapply(1:nrow(D4), function(i,mask){
        sapply(1:dim(D4)[3], function(j,mask){
          mask[!is.na(mask)]= D4[i,,j]
          mask[is.na(mask)]=0
          f.write.analyze(mask,paste(file,dimnames(D4)[[3]][j],sep="_",i),size="float")
        },mask)
      },mask)
    }
  }
  
  if(is.null(which.maps)){
    which.maps=names(D)
    which.maps=.get.maps.name(D) 
  }
  sapply(which.maps,function(i){.write.( D4=D[[i]],
                                         mask=D$mask,
                                         file=paste(file,sep="_",i))})
  
}
