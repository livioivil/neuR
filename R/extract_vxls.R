#'@description select voxels (or time courses of voxels) 
#'from a matrix of coordinates
#'@param xyz three type of inputs are allowed: 
#'a) nX3 matrix of n coordinates, 
#'b) vector of T/F on length equal to the number of voxels,
#'c) vector of indices of the voxels to keep.
#'@param D a neuR-object 
#'@param tcs any proper array e.g. from D@data$tcs
#'@param mask usually D@mask
#'@return a neuR-object 
#'@title extract_vxls extract_tc
#'@aliases extract_tc extract_vxls
#'@export extract_tc extract_vxls

### estrae timecourses e tutti gli elementi per una o piu' coordinate
extract_vxls <- function(xyz,D){
  if(is.vector(xyz)) {
    for(map in names(D@data) )
      D@data[[map]]=D@data[[map]][,xyz,,drop=FALSE]
    mask.id=D@mask[!is.na(D@mask)]
    mask.id=mask.id[xyz]
    } else {
    for(map in names(D@data) )
      D@data[[map]]=extract_tc(xyz,D@data[[map]],mask=D@mask)
    mask.id=apply(xyz,1,function(t){
      D@mask[t[1],t[2],t[3]]})
  }
  D@info$nvoxels=length(mask.id)
  D@info$dimnames[[2]]=D@info$dimnames[[2]][mask.id]
  mask=D@mask
  mask[]=NA
  mask[mask.id]=1:D@info$nvoxels
  D@mask=mask

  D
}


#seleziona un generico oggetto tcs
extract_tc<-function(xyz,tcs,mask){
  col.id=apply(xyz,1,function(t){
    mask[t[1],t[2],t[3]]})
  tcs=tcs[,col.id,,drop=FALSE]
  tcs
}


#' @description reduce a neuR-object 
#' @title pixelize
#' @param D a neuR-object
#' @param reduce.by (integer) resizing factor. 
#' If it is a vector, the tree coordinates indicate the 
#' rescaling to bi applied to each dimension. 
#' If it is a scalar, the same rescaling is applied to the 3 dims.
#' @export 
pixelize <- function(D,reduce.by=2){
  reduce.by=rep(reduce.by,length.out=3)
  new.size=dim(D)%/%reduce.by
  vertex.coordinates=expand.grid(
    seq(1,dim(D)[1]-reduce.by[1]+1,by=reduce.by[1]),
    seq(1,dim(D)[2]-reduce.by[2]+1,by=reduce.by[2]),
    seq(1,dim(D)[3]-reduce.by[3]+1,by=reduce.by[3]))
  
#   tcs=D@data$tcs
#   i2=10
#   i3=10
#   i1=10
#   
  xyz0=expand.grid((0:(reduce.by[1]-1)),
              (0:(reduce.by[2]-1)),
              (0:(reduce.by[3]-1)))
  .get.mean.tcs <- function(iii,xyz0=xyz0,tcs=tcs,na.rm=TRUE){ 
    #shifts the basic grid by coordinates c(i1,i2,i3)
    xyz=xyz0
    xyz[,1]=xyz[,1]+  vertex.coordinates[iii,1]
    xyz[,2]=xyz[,2]+  vertex.coordinates[iii,2]
    xyz[,3]=xyz[,3]+  vertex.coordinates[iii,3]
    
    #extract
    tcs=extract_tc(xyz,tcs=tcs,mask=D@mask)
    out=mclapply(1:dim(tcs)[3],function(i)
                 rowMeans(tcs[,,i],na.rm = na.rm))
    out=unlist(out)
  }

  D@data=parallel::mclapply (D@data,function(tc){
    res=mclapply(1:nrow(vertex.coordinates),.get.mean.tcs,xyz0=xyz0,tcs=tc)
    res=array(unlist(res),c(dim(tc)[1],prod(new.size),dim(tc)[3]))
  })
  D@info$dim.vol =new.size
  D@info$nvoxels = nrow(vertex.coordinates)
  D@info$dimnames[[2]]=rownames(vertex.coordinates)
  D@mask=array(1:D@info$nvoxels,new.size)
  D@info$header$dim [2:4] = new.size
D
}
