.cstp_avg<-function(array,w){
  my.mean<-function(x){
    weighted.mean(x,w)
  }
  avg=apply(array,c(1,2),my.mean)
  avg=(1/sum(w))*avg
  return(avg)
}