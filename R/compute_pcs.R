#' @name compute.pcs
#' @title Computes principal components for each voxel/channel of a neuR-object
#'
#' @description Computes principal components for each voxel/channel stored in tcs array of slot data of a neuR-object (i.e. D@data$tcs)
#' @param D a neuR-ogject
#' @param center TRUE 
#' @param scale FALSE
#' @param max.pc.num 1
#' @param drop.tcs TRUE
#' @param selected.volumes NULL or a logical vector of length n (number of volumes per block)
#' @param ... other 
#' @return a neuR-object
#' @export


compute.pcs <- function(D,center=TRUE,scale=FALSE,
                              max.pc.num=1,drop.tcs=TRUE,
                        selected.volumes=NULL,...){
  
  out <- .compute.pcs
  environment(out) <- sys.frame(sys.nframe())
  environment(out)$X <- D@data$tcs

  #estraggo al max max.pc.num comp principali. meno sono pi? veloce ? algoritmo.
  
  
#   environment(out) <- sys.frame(sys.nframe())
#   environment(out) <- list2env(dots(...))
  D@data=c(D@data,out())
  if(drop.tcs) D@data$tcs=NULL
  D
}

############### core function
.compute.pcs <- function(){
  pcs.col.names=colnames(X)
  if( !exists("selected.volumes")|| is.null(selected.volumes)) selected.volumes=rep(TRUE,nrow(X))
  sv.list=lapply(1:length(pcs.col.names), function(i) 
  {.extract.block.pc(X[selected.volumes,i,],
                     center=center,scale=scale,max.pc.num=max.pc.num)
  })
  #   str(sv.list)
  
  ##########
  .extract.pcs <- function(sv.list){
    pcs=array(sapply(sv.list,function(x,ob) x$pcs),
              c(dim(sv.list[[1]]$pcs),length(sv.list)))
    pcs=aperm(pcs,c(1,3,2))
    rownames(pcs)=dimnames(sv.list[[1]]$pcs)[[1]]
    colnames(pcs)=pcs.col.names
    dimnames(pcs)[[3]]=dimnames(sv.list[[1]]$pcs)[[2]]
    
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

#############
.set.default.params.compute.pcs <- function(out,...){
  dotss=pryr::dots(...)
  dotss=lapply(dotss,eval)
  
  if(is.null(names(dotss)))
    names(dotss)="X"
  
  if(is.null(dotss$X)&& sum(names(dotss)=="")>0)
    names(dotss)[which(names(dotss)=="")[1]]="X"
  
  
  if(is(dotss$X,"neuR.object"))
    dotss$X <- dotss$X@data$tcs
  if(is.null(dotss$center))
    dotss$center=TRUE
  if(is.null(dotss$scale))
    dotss$scale=FALSE
  if(is.null(dotss$max.pc.num))
    dotss$max.pc.num=1
  if(is.null(dotss$selected.volumes))
    dotss$selected.volumes=NULL
  
  environment(out) <-list2env(dotss)# sys.frame(sys.nframe())
  out
}