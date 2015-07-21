#creo una paletta colori e la setto come deault
pal.uno=c("#445577","#ffcc00","#00A08A","#FF0000","#45abff")
palette(pal.uno)

####################
#     reading, check and cleaning the mask file
.fix.mask<- function(mask,D4=NULL){
  #se è un file name legge il file
  if(is.character(mask))
    mask=f.read.analyze.volume(mask)  
  #forza ad essere un array 3D prendendo le dim da D4
  if(!is.null(D4))
    mask=array(mask,dim(D4)[1:3]) else
      #oppure le sue stesse dim
      mask=array(mask,dim(mask)[1:3])
  mask
}



##########
.cut.file.names <-function(files,max.length=15){
  nms=sapply(files,function(x)
        substr(x,start=max(1,nchar(x)-max.length+1),
               stop=nchar(x)))
  names(nms)=NULL
  nms
}

