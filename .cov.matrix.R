.cov.matrix<-function(trials.all,type="temporal"){
  if(type=="spatial")
    
    array.cov=array(NA,c(dim(trials.all)[2],dim(trials.all)[2],dim(trials.all)[3]))
  
  else{
    trials.all=aperm(trials.all,c(2,1,3))
    array.cov=array(NA,c(dim(trials.all)[2],dim(trials.all)[2],dim(trials.all)[3]))
    
  }
  
  for(i in 1:dim(array.cov)[3]){
    array.cov[,,i]=cov(trials.all[,,i])
  }
  
  array.cov
  
}