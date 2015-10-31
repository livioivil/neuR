# trasformo D in D3 (time X voxel/channel X block)
reshapeTcs2blocks <- function(D,blocks){
  if(is.null(blocks)) error("blocks can't be null")
  #if scalar, it is the number of volumes in each blocks
  if(length(blocks)==1)
    blocks=rep(1:(nrow(D@data$tcs)/blocks),each=blocks)
  
  D@data$tcs=.reshapeTcs2blocks(D@data$tcs,blocks)
  id.first.block=which(blocks==1)
  D@info$design=D@info$design[id.first.block,]
  D@info$ntimes=length(id.first.block)
  D
}
.reshapeTcs2blocks <- function(tcs,blocks){
  id.block.matrix=which(blocks>0)
  
  block.sizes=table(blocks[blocks>0])
  nblocks=length(block.sizes)
  #check if all block have the length:
  if(length(table(block.sizes))>1) error("not all blocks have the same sizes!")
  
    
  res=array(tcs[id.block.matrix,,,drop=FALSE],
            c(block.sizes[1],nblocks,ncol(tcs)))
  res=aperm(res,c(1,3,2))
  
  #aggiusto i nomi
  colnames(res)=colnames(tcs)
  names(dimnames(res))[1:2]=names(dimnames(tcs))[1:2]
  names(dimnames(res))[3]="blocks"
  dimnames(res)[[3]]=paste("bk",sep="",1:dim(res)[3])
  res
}
