.sum.matrix<-function(dots.array){
  somma=matrix(0,nrow=dim(dots.array)[1],ncol=dim(dots.array)[2])
  for(i in 1:dim(dots.array)[3]){
    somma=somma+dots.array[,,i]
  }
  somma
}