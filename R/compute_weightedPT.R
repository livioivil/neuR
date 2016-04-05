#' @name compute_weighted_P
#' @title Computes weighted p-values (and T) of a neuR-object
#'
#' @description Computes IRC (intra run correlation) of a neuR-object pcs array of slot data of a neuR-object (i.e. D@data$pcs)
#' @param D a neuR-object
#' @param Pmap charachter (i.e. the name of the map in D) or an array. "test.P" by default.
#' @param Wmap charachter (i.e. the name of the map in D) or an array. "IRC" by default.
#' @param tail the sign of the alternative. It can be 0, 1, -1.
#' @param SignMap (only needed if tail=0) the name of the map that has the sign of the test statistic. "test.T" by default
#' @param return can be "Pmap", "Zmap" or c("Pmap","Zmap")
#' @return Maps a named list of array/s
#' @export compute_weighted_P

compute_weighted_P<- function(D,Pmap="test.P",Wmap="IRC",
                              returnMaps="P",names_out=c(Pmap=paste("test.P_W",Wmap,sep=""),
                                                     Zmap=paste("test.T_W",Wmap,sep="")),
                              tail=NULL,SignMap="test.T"){
     out <- .compute.irc
     environment(out) <- sys.frame(sys.nframe())
     environment(out)$var=D@data$var[,,pc.num,drop=FALSE]
     environment(out)$var.tot=D@data$var.tot
     D=add.neuR.map(D,out(),out.name = "irc")
     if(drop.tcs) D@data$tcs=NULL
     D
}

.compute_weighted_P <- function(Pmap,Wmap,returnMaps,names_out,tail,SignMap){
  # estrae la percentuale di var spiegata dalla prima componente (su una singola decomposizione, singolo voxel)  
    WP=pmin(D@data[[Pmap]]/(D@data[[Wmap]]/mean(D@data[[Wmap]])),1)
    WP[is.na(WP)]=1
   out=list(WP)
   if("Zmap"%in%returnMaps){
     WT=P2T(D,tail=tail,
            PMap="spmP",SignMap=SignMap)
   }
   names(out)=names_out
}

# .set.default.params.compute.wp <- function(out,...){
#   dotss=pryr::dots(...)
#   dotss=mclapply(dotss,eval)
#   
#   if(is.null(names(dotss)))
#     names(dotss)="var"
#   
#   if(is.null(dotss$var)&& sum(names(dotss)=="")>0)
#     names(dotss)[which(names(dotss)=="")[1]]="var"
#   
#   if(is.null(dotss$pc.num))
#     dotss$pc.num=1
#   if(is(dotss$var,"neuR.object")){
#     dotss$var=D@data$var[,,dotss$pc.num,drop=FALSE]
#     dotss$var.tot=D@data$var.tot
#   }
#   if(is.null(dotss$var))
#     dotss$var=D@data$var[,,dotss$pc.num,drop=FALSE]
#   if(is.null(dotss$var.tot))
#     dotss$var.tot=D@data$var.tot
#   
#   environment(out) <-list2env(dotss)# sys.frame(sys.nframe())
#   out
# }