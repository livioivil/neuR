#'@title scale.neuR
#'@aliases scale.neuR
#'@aliases .inv.sqrtM2
#'@description same as scale but does not divide
#'

# .inv.sqrtM2 <- function(X)  sqrt(colSums(X^2))^-1

scale.neuR <- function(X,center=TRUE, scale=FALSE){
  X=scale(X,center=center,scale = FALSE)
  if(scale){
    .sqrtM2 <- function(X)  X/sqrt(mean(X^2))
     X=apply(X,2,.sqrtM2)
  }
  X
}