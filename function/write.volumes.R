write.volumes <- function(D,which.maps="tcs",file.root.name="V"){

  #generica funzione che scrive i volumi di una matrice D time X voxel X block (oppue x pc)
  .write. <- function(D4,mask,file.name){
    if(is.null(mask)){ #allora ci deve essere almeno dim.vol
      D4=array(D4,c(nrow(D4),dim.vol,dim(D4)[3]))
      sapply(1:nrow(D4), function(i)
        sapply(1:dim(D4)[3], function(j){
          f.write.analyze(D4[i,,,,j],paste(file.name,i,sep=""),size="float")
        })      )
    } else {
      sapply(1:nrow(D4), function(i,mask){
        sapply(1:dim(D4)[3], function(j,mask){
          mask[!is.na(mask)]= D4[i,,j]
          mask[is.na(mask)]=0
          f.write.analyze(mask,paste(file.name,dimnames(D4)[[3]][j],sep="_",i),size="float")
        },mask)
      },mask)
    }
  }
  
  if(is.null(which.maps)){
    which.maps=names(D@data)
   }
  
  sapply(which.maps,function(map.i){
    .write.( D4=get.neuR.map(D,map.i),
             mask=D@mask,
             file.name=paste(file.root.name,sep="_",map.i))})

}
