permute.columns <- function(X){
  apply(X,2,function(x)sample(x))
}


### random or all slide of time-courses
slide.1.column<- function(x,startFrom,n){
  fromStepToN=startFrom:n
  c(x[fromStepToN],x[-fromStepToN])
}

slide.columns <- function(X,startFrom,n){
  temp=cbind(as.vector(X[,,1]),
        sapply(1:length(startFrom), function(i)
          X[slide.1.column(1:n,startFrom[i],n),,i+1])
        )
  temp=array(temp,dim(X))
  temp
}
