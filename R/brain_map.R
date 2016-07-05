#' @description Function that realizes a simple map of the head 
#'              (topview).
#'              Funzione che produce una mappa stilizzata della testa
#'              vista dall'alto. Di default plotta 20 canali 
#'              con coordinate prefissate. 
#'              
#' @title brain map of given values 
#' @name brain_map
#' @param values vector containig p-values that are to be drawn 
#'              relative to channels.
#'              It has to have the same length of the vector containig
#'              the coordinates of the channels.
#'              vettore contenente i p-value relativi ai canali da plottare.
#'              Deve avere la stessa lunghezza del vettore contenente le 
#'              coordinate in cui plottare i canali.
#' @param labels vector containig the names of the channels 
#'               that are to be drawn. If NULL, it take the labels from names(values)
#'               vettore contenente i nomi dei canali da plottare.
#' @param main the title of the plot. NULL by default
#'             titolo del plot NULL di default. 
#' @param coords matrix of two columns containing the abscissa and the ordinates of the 
#'               points that are to be drawn. Value's range [-1,1]
#'               default: channels are drawn at fixed coordinates
#'               vettore contenente le ascisse dei punti da plottare.
#'               I valori devono essere compresi tra [-1,1]. Di default 
#'               i punti sono plottati a coordinate predefinite.
#' @author Michelle Viola
#' @export brain_map

brain_map<-function(values,labels=NULL,main=NULL,coords=NULL,col=c("white","red")[(values<.05)+1]){
  if(is.null(coords)){
    coords=cbind(x=c(-0.5,-0.2,-0.3,-0.65,-0.75,-0.3,-0.5,-0.2,-0.75,-0.65,0.5,0.2,0.3,0.65,0.75,0.3,0.5,0.2,0.75,0.65),
                 y=c(0.7,0.45,0.1,0.1,0.45,-0.1,-0.7,-0.45,-0.45,-0.1,0.7,0.45,0.1,0.1,0.45,-0.1,-0.7,-0.45,-0.45,-0.1)
    )
    rownames(coords)=c("A1","A2","A3","A7","A8","B3","B4","B5","B6","B7","C1","C2","C3","C7","C8","D3","D4","D5","D6","D7")
  }
  if(is.null(labels)){labels=rownames(coords)}
  
  how_many_points=nrow(coords)
  if(is.null(labels)) labels=names(values)
  
  x=seq(from=0,to=2*pi,by=0.01)
  y=seq(from=0,to=2*pi,by=0.01)

  plot(cos(x),sin(y),type="l",asp=1,xlab=" ",ylab=" ",axes=F,main=main)
  space=cbind(coords,values)
    #naso
    points(0,1,pch=2,cex=2)
    text(-1.2,0,labels="Left",adj=0,cex=1)
    text(1.05,0,labels="Right",adj=0,cex=1)
    text(0,0.7,labels="Ant",adj=0,cex=1)
    text(0,-0.7,labels="Post",adj=0,cex=1)
    
    
    if(col=="heat.colors") {
      palette(heat.colors(32))
      col=ceiling((values-min(values)*.9999)/(max(values)-min(values)*.9999)*32)
    }
    points(space[,1],space[,2],cex=3,pch=21,bg=col)
    text(space[,1],space[,2],labels,adj=0,pos=4,cex=1)
  
    # if(adj==T){
    #   legend(c(0.2,1),legend=c("adj.p-val<0.05","adj.p-val>0.05"),bty="n",pch=c(rep(21,2)),pt.bg=c("red","white"),pt.cex=2,cex=0.8,y.inters=0.8)
    # }else{
    #   legend(c(0.2,1),legend=c("p-val<0.05","p-val>0.05"),bty="n",pch=c(rep(21,2)),pt.bg=c("red","white"),pt.cex=2,cex=0.8,y.inters=0.8)
    # }
}
