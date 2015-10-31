library(parallel)
#extract the singular values of X
# if slide=FALSE, use random permutations
test.irc <-function(X,perms=1000,seed=NULL,
                    center=TRUE,scale=FALSE,
                    rescale.proportion=TRUE,
                    slides=TRUE){
  
  ######################
  .compute.irc.fast <- function(X){
    unlist(mclapply(1:ncol(X), 
           function(i)  .extract.block.svd.d(X[,i,] )[1]))
  }
  ######################
  dim3=dim(X)[3]
  nrows=nrow(X)
  
  # set the seed if required, for reproducibility
  if(!is.null(seed)) set.sed(seed)

  pcs.col.names=colnames(X)
  if(any(center,scale))
    X=apply(X,c(2,3),scale,center,scale)
  
  if(slides){ 
    if(perms>=nrows^(dim3-1)) { # if it is possibile to compute all permutations:
      temp=mclapply(1:(dim3-1),function(i)1:nrows)
      all.slides <- as.matrix(expand.grid(temp))
      
      Tstats=mclapply(1:nrow(all.slides),function(id){
        X=slide.columns(X,startFrom=all.slides[id,],n=nrows)
        eival.obs=.compute.irc.fast(X)})
    } else  {
      Tstats=mclapply(1:perms,function(i){
        X=slide.columns(X,startFrom=sample(nrows,dim3-1),n=nrows)
        eival.obs=.compute.irc.fast(X)})
      Tstats=c(list(.compute.irc.fast(X)),Tstats)
    }  
    } else{
      
    Tstats=mclapply(1:perms,function(i){
      X=permute.columns(X,dim3,nrows)
      eival.obs=.compute.irc.fast(X)})
    Tstats=c(list(.compute.irc.fast(X)),Tstats) 
  }
  
  Tstats=t(as.matrix(data.frame(Tstats)))
  dimnames(Tstats)=NULL
  
  Tstats=Tstats^2
  
  if(rescale.proportion) Tstats=Tstats%*%diag(1/rowSums(colSums(X^2)))
  
  p.vals=colMeans(Tstats>=matrix(Tstats[1,],byrow = TRUE,nrow = nrow(Tstats),ncol = ncol(Tstats)))
  p.vals
}