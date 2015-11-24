#' @name read.fMRI.data
#' @title Reads .nii and .img/.hdr file data and save to neuR-object
#'
#' @description Reads .nii and .img/.hdr file data and save to neuR-object
#' @param path "."
#' @param pattern "s.*\\.img"
#' @param files NULL
#' @param mask a file name, a array, equal to 'constant' (look for non constant voxels) 
#' or a scalar (values equal to this number are out of the brain) 
#' @param info NULL
#' @param silent FALSE
#' @param exclude.files vector of ids of files to exclude
#' @return a neuR-object
#' @export


read.fMRI.data <- function(path=".",pattern="s.*\\.img",files=NULL,mask='constant',
                          info=NULL,silent=FALSE,exclude.files=c()){
  
  if(is.null(files))  {
    files=dir(path,pattern=pattern,full.names =TRUE)
  } else{
    files=paste(path,files,sep="/")
  }
  files=gtools::mixedsort(files)
  if(!is.null(exclude.files)&&length(exclude.files)>0)
    files=files[-exclude.files]
  if(!silent) cat("\n reading:",files[1]," ..")
  n=nchar(files[1])
  if(substr(files[1],n-2,n)=="nii"){
    f.read.vol=AnalyzeFMRI::f.read.nifti.volume
    f.read.head=AnalyzeFMRI::f.read.nifti.header
    } else{
      f.read.vol=AnalyzeFMRI::f.read.analyze.volume
      f.read.head=AnalyzeFMRI::f.read.analyze.header
    }
  
  tt=f.read.vol(files[1])
  tt.head=f.read.head(files[1])
  nfiles=length(files)
  TT=array(NA,c(dim(tt)[1:3],nfiles))
  TT[,,,1]=tt
  if( nfiles >1){
    for(i in 2:nfiles){
      if(!silent) cat("\n reading:",files[i]," ..")
      tt=f.read.vol(files[i])
      TT[,,,i]=tt
    }
  }
  
  if(is.character(mask)){
    if(mask=='constant'){
      # try to detect automatically: (non-constant)  
      if(nfiles>1) {
        mask=apply(TT,1:3,function(x)length(unique(x))>1)*1
      } else 
        warning("There is only one volume, mask can not detected for option mask='constant'")
   } else # it is a file name, read it:
        mask=f.read.vol(mask)
  } 
  if(length(mask)==1) #is a scalar
    {
    if(nfiles==1)
      mask=TT!=mask 
    else  
      mask=(apply(TT,1:3,prod)!=mask)*1
     }# otherwise it is an array.
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
  info$dimnames=dimnames(tcs)
  
  res <- new("neuR.object")
#   dimnames(tcs)=NULL
  res@data$tcs=tcs
  res@mask=mask
  res@info=info
  res
}
