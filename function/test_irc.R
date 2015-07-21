#extract the singular values of X
test.irc <-function(X,perms=1000,seed=NULL,
                    center=TRUE,scale=FALSE,
                    rescale.proportion=TRUE){
  
  ######################
  compute.irc.fast <- function(X){
    sapply(1:ncol(X), 
           function(i)  .extract.block.svd.d(X[,i,] ))[1,]
  }
  ######################
  dim3=dim(X)[3]
  nrows=nrow(X)
  
  # set the seed if required, for reproducibility
  if(!is.null(seed)) set.sed(seed)

  pcs.col.names=colnames(X)
  if(any(center,scale))
    X=apply(X,c(2,3),scale,center,scale)

   convertToBaseN <-function(id,N){
     id%/%N
   }
   Tstats=t(replicate(perms,{
     X=slide.columns(X,startFrom=sample(nrows,dim3-1),n=nrows)
     eival.obs=compute.irc.fast(X)}))
  
  Tstats=rbind(obs=compute.irc.fast(X),
               Tstats)
  
  Tstats=Tstats^2
  
  if(rescale.proportion) Tstats=Tstats/rowSums(colSums(X^2))
  
  p.vals=colMeans(Tstats>=matrix(Tstats[1,],byrow = TRUE,nrow = nrow(Tstats),ncol = ncol(Tstats)))
}