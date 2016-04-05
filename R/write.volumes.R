#' @name write.volumes
#' @title writes  .nii and .img/.hdr file file from a neuR-object
#'
#' @description writes  .nii and .img/.hdr file file from a neuR-object. occhio agli header, per ora file.type pari al formato della immagine che ha generato l'header.
#' @param D neuR-object
#' @param which.maps by default: all maps in D
#' @param file.root.name "V"
#' @param file.type "nifti"
#' @param into.path "."
#' @return NULL
#' @export

write.volumes <- function(D,which.maps = NULL,
                          file.root.name = "V",
                          file.type = "nifti",
                          into.path = "."){
  
 # generica funzione di scrittura di immagini
  .get.write.function <- function(file.type,D){
    if(file.type=="nifti"){
      f= function(data.vol,filename){
        AnalyzeFMRI::f.write.nifti(data.vol,filename,
                      L = D@info$header,nii = TRUE)
      }
      } else if(file.type=="analyze") {
        f= function(data.vol,filename){
          AnalyzeFMRI::f.write.nifti(data.vol,filename,
                                     L = D@info$header,nii = FALSE)
#           AnalyzeFMRI::f.write.analyze(data.vol,filename,size="float")
          AnalyzeFMRI::f.write.list.to.hdr(L= D@info$header,file=paste(filename,".hdr",sep=""))
          
      }
      } else {
        warning("Unknown file.type",call. = TRUE)
      }
  
    f
  }
  #generica funzione che scrive i volumi di una matrice D time X voxel X block (oppue x pc)
  .write. <- function(D4,mask,file.name,f.write.neuR){
    if(is.null(mask)){ #allora ci deve essere almeno dim.vol
      D4=array(D4,c(nrow(D4),dim.vol,dim(D4)[3]))
      mclapply(1:nrow(D4), function(i)
        mclapply(1:dim(D4)[3], function(j){
          f.write.neuR(D4[i,,,,j],paste(file.name,i,sep=""))
        })      )
    } else {
      sapply(rownames(D4), function(i,mask){
        sapply(1:dim(D4)[3], function(j,mask){
          mask[!is.na(mask)]= D4[i,,j]
          mask[is.na(mask)]=0
          f.write.neuR(mask,paste(file.name,dimnames(D4)[[3]][j],sep="_",i))
        },mask)
      },mask)
    }
  }
  
  if(is.null(which.maps)){
    which.maps=names(D@data)
   }

  
  f.write.neuR <- .get.write.function(file.type,D)
  # creates the dir, if needed
  dir.create(into.path,recursive = TRUE)
  sapply(which.maps,function(map.i){
    D4 <- get.neuR.map(D,map.i)
    D4 <- .fix.names.neuR.data(D4)
    .write.( D4=D4,
             mask=D@mask,
             file.name=paste(into.path, sep="/",
                             paste(file.root.name,sep="_",map.i)),
             f.write.neuR=f.write.neuR)})
return(NULL)
}
