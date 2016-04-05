.cstp_weight<-function(filtri.list,dots){
  B=filtri.list[[1]]
  D=filtri.list[[2]]
  A=filtri.list[[3]]
  E=filtri.list[[4]]
  
  wtemp=0
  w=0
  
  for(i in 1:dim(dots)[3]){
    wtemp[i]=norm(E%*%t(D)%*%dots[,,i]%*%B%*%t(A),type="F")/norm(dots[,,i]-(E%*%t(D)%*%dots[,,i]%*%B%*%t(A)),type="F")
  }
  wtemp
  for(i in 1:length(wtemp)){
    w[i]=(dim(dots)[3]*wtemp[i])/sum(wtemp)
  }
  return(w)
}