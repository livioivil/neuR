#' @name read.fMRI.data
#' @title Reads .nii and .img/.hdr file data and save to neuR-object
#'
#' @description Reads .nii and .img/.hdr file data and save to neuR-object
#' @param path "."
#' @param pattern "s.*\\.img"
#' @param files NULL
#' @param mask NULL
#' @param info NULL
#' @return a neuR-object
#' @export

read.fMRI.data <- function(path=".",pattern="s.*\\.img",files=NULL,mask=NULL,
                          info=NULL){
  
  if(is.null(files))  {
    files=dir(path,pattern=pattern,full.names =TRUE)
  } else{
    files=paste(path,files,sep="/")
  }
  cat("\n reading:",files[1]," ..")
  n=nchar(files[1])
  if(substr(files[1],n-2,n)=="nii"){
    f.read.vol=f.read.nifti.volume
    f.read.head=f.read.nifti.header
    } else{
      f.read.vol=f.read.analyze.volume
      f.read.head=f.read.analyze.header
    }
  
  tt=f.read.vol(files[1])
  tt.head=f.read.head(files[1])
  nfiles=length(files)
  TT=array(NA,c(dim(tt)[1:3],nfiles))
  TT[,,,1]=tt
  if( nfiles >1){
    for(i in 2:nfiles){
      cat("\n reading:",files[i]," ..")
      tt=f.read.vol(files[i])
      TT[,,,i]=tt
    }
  }
  
  if(is.null(mask)) # try to detect automatically: (zero-constant)
    mask=apply(TT,1:3,function(x)length(unique(x))>1)*1 else
      if(is.character(mask)) #if it is a file name, read it:
        mask=f.read.vol(mask)
  # otherwise it is an array.
  # force mask to have the same dims of dim(TT)[1:3]
  mask=array(as.integer(mask),dim(TT)[-4])
  
  #the volume has NAs outside the mask, while ids 1:V into the mask
  mask[mask>0]=1:sum(mask>0)
  mask[mask==0]=NA

  
  coord.mask=which(!is.na(mask),arr.ind =TRUE)
  tcs=apply(coord.mask,1,function(coord)TT[coord[1],coord[2],coord[3],])
  if(is.vector(tcs)) tcs=matrix(tcs,1,length(tcs))
  rm(TT,coord.mask)
  
  tcs=array(tcs,c(dim(tcs),1))
  rownames(tcs)=.cut.file.names(files,15)
  colnames(tcs)=paste("v",sep=".",1:ncol(tcs))
  dimnames(tcs)[3]="tc"
  
  #overwrite the old dims
  info$dim.vol=dim(mask)
  info$ntimes=nrow(tcs)
  info$nvoxels=sum(!is.na(mask))
  info$header=tt.head

  res <- new("neuR.object") 
  res@data$tcs=tcs
  res@mask=mask
  res@info=info
  res
}
