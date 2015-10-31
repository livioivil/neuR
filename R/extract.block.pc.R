### calcola pcs per singolo time course 
.extract.block.pc <- function(tcblocks,
                             center=TRUE,scale=TRUE,
                             max.pc.num=1,...){
  #(centro e) riscalo ma solo se richiesto (pari a cor=TRUE)
  tcblocks=scale(tcblocks,center=center, scale=scale)
  if(any(!is.finite(tcblocks))) {
    sv$d=rep(NA,max.pc.num)
    sv$u=matrix(NA,nrow(tcblocks),max.pc.num)
    sv$v=matrix(NA,ncol(tcblocks),max.pc.num)
    sv$var.tot=NA    
  } else{
    #estraggo al max max.pc.num comp principali. meno sono pi? veloce ? algoritmo.
    sv=svd(tcblocks,nu=max.pc.num,nv=max.pc.num)
    ##
    sv=check.flip.sign(sv)
    if(max.pc.num==1) sv$u=sv$u*sv$d[1] else 
      sv$u=sv$u%*%diag(sv$d[1:max.pc.num])
    sv$d=sv$d^2/(nrow(sv$u)-1)
    sv$var.tot=sum(sv$d)
    sv$d=sv$d[1:max.pc.num]
  }
  names(sv)=c("var","pcs","loadings","var.tot")
  colnames(sv$pcs)<-colnames(sv$loadings)<-paste("pc",sep=".",1:ncol(sv$pcs))
  
  sv
}
