.cstp_cov<-function(trials.all,type="temporal"){
  
  pesi=1/dim(trials.all)[3]
  
  if(type=="spatial"){
    
    sum.cov.t=.sum.matrix(.cov.matrix(trials.all,type=type))
    cov.media=pesi*sum.cov.t
  }
  else{
    
    sum.cov.s=.sum.matrix(.cov.matrix(trials.all,type=type))
    cov.media=pesi*sum.cov.s
  }
}