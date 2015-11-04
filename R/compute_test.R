#' @name compute.test
#' @title Computes Tests in a neuR-object
#'
#' @description Computes tests of a neuR-object pcs array of slot data of a neuR-object (i.e. D@data$pcs)
#' @param D a neuR-object
#' @param left.array the name of the map in D@data to be used in the model or the array itself
#' @param right.formula ~1 by default. NOT IMPLEMENTED YET. The names refer to the columnames of D@info$design 
#' @param offset.values (NULL by default, same effect as 0) A value to be subtracted to each value of the matrix (same dims of left.array)
#'        It may also be 'meanOverall' or 'meanBySubject' (i.e. average over the second dimension)
#' @return a list of 3D arrays, usually a T and P one.
#' @export

compute.test <- function(D,left.array,right.formula = ~1, offset.values = NULL,
                         tail=1){
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
  
  out=.t.test.1.light(left.array,tail)
  attr(out$T,"df")=out$df
  out$df=NULL
  out
}

.t.test.1.light <- function(Y,tail=1){
  meanY=colMeans(Y,na.rm=TRUE)
  if(any(is.na(Y))) {
    n=colSums(!is.na(Y)) 
  } else {
    n=nrow(Y)
  }
  sdY=((colSums(Y^2,na.rm=TRUE)-n*(meanY)^2)/(n-1))^.5
  meanY=array(meanY,c(1,dim(meanY)),dimnames = c("",dimnames(meanY)))
  sdY=array(sdY,dim(meanY))
  dimnames(sdY)=dimnames(meanY)
  ts= meanY/sdY*sqrt(n)
  if(tail==0)  return(list(p=2*pt(-abs(ts),df = n-1),t=ts,
                           Mean=meanY,Sd=sdY,df=n-1))
  return(list(P=pt(-sign(tail)*ts,df = n-1),T=ts,
              Mean=meanY,Sd=sdY,df=n-1))
}

.set.default.params.compute.test <- function(out,...){
  dotss=pryr::dots(...)
  dotss=lapply(dotss,eval)
  
  dotss$left.array=get.neuR.map(obj = dotss$D,map = dotss$left.array)
  
  if(is.null(dotss$tail))
    dotss$tail=1
  if(is.null(dotss$offset.values))
      dotss$offset.values=0
  
  environment(out) <-list2env(dotss)# sys.frame(sys.nframe())
  out
}