#' @description Function that realizes a simple map of the head 
#'              (topview).
#'              Funzione che produce una mappa stilizzata della testa
#'              vista dall'alto. Di default plotta 20 canali 
#'              con coordinate prefissate. 
#'              
#' @title Mappa dei canali ai quali
#'        è associato un p-value 
#'        statisticamente significativo. 
#' @name mappa
#' @param p.val vector containig p-values that are to be drawn 
#'              relative to channels.
#'              It has to have the same length of the vector containig
#'              the coordinates of the channels.
#'              vettore contenente i p-value relativi ai canali da plottare.
#'              Deve avere la stessa lunghezza del vettore contenente le 
#'              coordinate in cui plottare i canali.
#' @param labels vector containig the names of the channels 
#'               that are to be drawn
#'               vettore contenente i nomi dei canali da plottare.
#' @param main the title of the plot. NULL by default
#'             titolo del plot NULL di default. 
#' @param coordx vector containing the abscissa of the 
#'               points that are to be drawn. Value's range [-1,1]
#'               default: channels are drawn at fixed coordinates
#'               vettore contenente le ascisse dei punti da plottare.
#'               I valori devono essere compresi tra [-1,1]. Di default 
#'               i punti sono plottati a coordinate predefinite.
#' @param coordy  vector containing the ordinates of the 
#'               points that are to be drawn. Value's range [-1,1]
#'               default: channels are drawn at fixed coordinates
#'               vettore contenente le ordinate dei punti da plottare.
#'               I valori devono essere compresi tra [-1,1]. Di default 
#'               i punti sono plottati a coordinate predefinite.
#' @param adj if TRUE, p-values are corrected with the "Bonferroni" method
#'        if FALSE, no correction is made.
#' @author Michelle Viola
#' @export
#' 
mappa<-function(p.val,labels,main=NULL,coordx=NULL,coordy=NULL,adj=F){
  eti=labels
  main=main
  how_many_points=length(coordx)
  x=seq(from=0,to=2*pi,by=0.01)
  y=seq(from=0,to=2*pi,by=0.01)
  if(adj==T){
    values=p.adjust(p.val,method="bonferroni")
  }else{
    values=p.val
  }
  
    plot(cos(x),sin(y),type="l",asp=1,xlab=" ",ylab=" ",axes=F,main=main)
    
    if((is.null(coordx)&&(is.null(coordy)))){
      coordx=c(-0.5,-0.2,-0.3,-0.65,-0.75,-0.3,-0.5,-0.2,-0.75,-0.65,0.5,0.2,0.3,0.65,0.75,0.3,0.5,0.2,0.75,0.65)
      coordy=c(0.7,0.45,0.1,0.1,0.45,-0.1,-0.7,-0.45,-0.45,-0.1,0.7,0.45,0.1,0.1,0.45,-0.1,-0.7,-0.45,-0.45,-0.1)
    }else{
      coordx=coordx
      coordy=coordy
      }
    
    space=cbind(coordx,coordy,values)
    #naso
    points(0,1,pch=2,cex=2)
    text(-1.3,0,labels="Left",adj=0,cex=1)
    text(1.3,0,labels="Right",adj=0,cex=1)
    text(0,0.7,labels="Ant",adj=0,cex=1)
    text(0,-0.7,labels="Post",adj=0,cex=1)
    for(j in 1:length(coordx)){
      if((values[j]<0.05)){temp.col="red"}
      if(values[j]>0.05){temp.col="white"}
      points(space[j,1],space[j,2],cex=3,pch=21,bg=temp.col)
      text(space[j,1],space[j,2],eti[j],adj=0,pos=4,cex=1)
    }
    if(adj==T){
      legend(c(0.2,1),legend=c("adj.p-val<0.05","adj.p-val>0.05"),bty="n",pch=c(rep(21,2)),pt.bg=c("red","white"),pt.cex=2,cex=0.8,y.inters=0.8)
    }else{
      legend(c(0.2,1),legend=c("p-val<0.05","p-val>0.05"),bty="n",pch=c(rep(21,2)),pt.bg=c("red","white"),pt.cex=2,cex=0.8,y.inters=0.8)
    }
}
