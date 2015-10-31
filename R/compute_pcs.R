### come extract.block.pc, ma sui dati D
compute.pcs <- function(D,center=TRUE,scale=FALSE,
                              max.pc.num=1,drop.tcs=TRUE,...){
  #estraggo al max max.pc.num comp principali. meno sono pi? veloce ? algoritmo.
  pcs.col.names=colnames(D@data$tcs)
  sv.list=lapply(1:length(pcs.col.names), function(i) 
    {.extract.block.pc(D@data$tcs[,i,],
                center=center,scale=scale,max.pc.num=max.pc.num)
  })
#   str(sv.list)
  if(drop.tcs) D@data$tcs=NULL
##########
  .extract.pcs <- function(sv.list){
    pcs=array(sapply(sv.list,function(x,ob) x$pcs),
        c(dim(sv.list[[1]]$pcs),length(sv.list)))
    pcs=aperm(pcs,c(1,3,2))
    rownames(pcs)=dimnames(sv.list[[1]]$pcs)[[1]]
    colnames(pcs)=pcs.col.names
    dimnames(pcs)[3]=dimnames(sv.list[[1]]$pcs)[[2]]
    
    pcs
  }
  pcs=.extract.pcs(sv.list)
  
  #rimuovo le pcs dalla lista, per evitare replicati
  sv.list=lapply(sv.list, function(i) {i$pcs=NULL;i})
  #str(sv.list)
  
##########  
  .extract.loadings <- function(sv.list){
    loadings=array(sapply(sv.list,function(x,ob) x$loadings),
              c(dim(sv.list[[1]]$loadings),length(sv.list)))
    loadings=aperm(loadings,c(1,3,2))
    colnames(loadings)=pcs.col.names
    dimnames(loadings)[[3]]=dimnames(sv.list[[1]]$loadings)[[2]]
    loadings
  }
  loadings=.extract.loadings(sv.list)
  #rimuovo le pcs dalla lista, per evitare replicati
  sv.list=lapply(sv.list, function(i) {i$loadings=NULL;i})

########## 
  .extract.var <- function(sv.list){
    var=array(sapply(sv.list,function(x,ob) x$var),
                   c(length(sv.list[[1]]$var),length(sv.list)))
    var=array(var,c(dim(var),1))
    colnames(var)=pcs.col.names
    rownames(var)=colnames(sv.list[[1]]$pcs)
    var=aperm(var,c(3,2,1))
  }
  var=.extract.var(sv.list)
  
##########
  var.tot=sapply(sv.list,function(x) x$var.tot)
  var.tot=array(var.tot,c(1,length(var.tot),1))
  dimnames(var.tot)[[2]]=pcs.col.names

  list(pcs=pcs,loadings=loadings,var=var,var.tot=var.tot)
}