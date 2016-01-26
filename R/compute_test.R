#' @name compute.test
#' @title Computes Tests in a neuR-object
#'
#' @description Computes tests of a neuR-object pcs array of slot data of a neuR-object (i.e. D@data$pcs)
#' @param D a neuR-object
#' @param left.array the name of the map in D@data to be used in the model or the array itself
#' @param right.formula ~1 by default. NOT IMPLEMENTED YET. The names refer to the columnames of D@info$design 
#' @param offset.values (NULL by default, same effect as 0) A value to be subtracted to each value of the matrix (same dims of left.array)
#'        It may also be 'meanOverall' or 'meanBySubject' (i.e. average over the second dimension)
#' @param drop.tcs FALSE
#' @param avoidNullPvalues TRUE by default. It substitute 0s with the min(1E-5, minimum value greater than 0 in the vector of p-values)
#' @return a list of 3D arrays, usually a T and P one.
#' @export compute.test

compute.test <- function(D,left.array,right.formula = ~1, offset.values = NULL,
                         tail=1,drop.tcs=FALSE,avoidNullPvalues=TRUE){
     out <- .compute.test
     environment(out) <- sys.frame(sys.nframe())
     D@data=c(D@data,test=out())
     if(drop.tcs) D@data$tcs=NULL
     D
}

.compute.test <- function(){
  # center the data array
  if(!is.null(offset.values)){
    if(offset.values=='meanOverall') {
      left.array = left.array-mean(left.array)
    } else if(offset.values=='meanBySubject'){
      left.array = aperm(apply(left.array,c(1,3),function(x) x-mean(x)),c(2,1,3))
    } else {
      left.array = left.array-offset.values
    }
  }
  
  out=.t.test.1.light(left.array,tail,avoidNullPvalues)
  attr(out$T,"df")=out$df
#   out$df=NULL
  out
}

.t.test.1.light <- function(Y,tail=0,avoidNullPvalues=TRUE){
  meanY=colMeans(Y,na.rm=TRUE)
  if(any(is.na(Y))) {
    n=colSums(!is.na(Y)) 
  } else {
    n=nrow(Y)
  }
  sdY=((colSums(Y^2,na.rm=TRUE)-n*(meanY)^2)/(n-1))^.5
  meanY=array(meanY,c(1,dim(Y)[-1]),dimnames = c("",dimnames(meanY)))
  rm(Y)
  sdY=array(sdY,dim(meanY))
  dimnames(sdY)=dimnames(meanY)
  ts= meanY/sdY*array(sqrt(n),c(1,dim(meanY)[2],dim(meanY)[3]))
  n=array(n,dim(meanY)) 
  
  if(tail==0){  
    P=2*pt(-abs(ts),df = n-1) 
    } else{
      P=pt(-sign(tail)*ts,df = n-1)
    }
  
  if(avoidNullPvalues) P[P==0]=min(c(1E-5,P[P!=0]),na.rm = TRUE)
  return(list(P=P,T=ts,
              Mean=meanY,Sd=sdY,df=n-1))
}

.set.default.params.compute.test <- function(out,...){
  dotss=pryr::dots(...)
  dotss=lapply(dotss,eval)
  if(is.character(dotss$left.array))
    dotss$left.array=get.neuR.map(obj = dotss$D,map = dotss$left.array)
  
  if(is.null(dotss$tail))
    dotss$tail=1
  if(is.null(dotss$offset.values))
      dotss$offset.values=0
  if(is.null(dotss$avoidNullPvalues))
    dotss$avoidNullPvalues=TRUE
  
  environment(out) <-list2env(dotss)# sys.frame(sys.nframe())
  out
}