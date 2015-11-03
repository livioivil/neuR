#' @name get.neuR.map
#' @title get functions to compute maps neuR-object
#' @description NOT IMPLEMENTED YET: out.funct.name=map the name of the function to be generated (eg different names depending on the parameters)
#' @param funct 
#' @param ... are used to set the parameters of the map 
#' @return a function
#' @export

get.neuR.funct <- function(funct,...){
  if (length(funct)>1) {
    out=lapply(funct,get.neuR.funct)
    } else
      {  out=switch(funct, 
                    pcBlocks = {out <- .compute.pcs
                                environment(out) <- sys.frame(sys.nframe())
                                environment(out)$X <- D@data$tcs
                                out}, 
                    irc = {out <- .compute.irc
                           environment(out) <- sys.frame(sys.nframe())
                           environment(out)$var=D@data$var[,,pc.num,drop=FALSE]
                           environment(out)$var.tot=D@data$var.tot
                           out}, 
                    irh = {out <- .compute.irh
                           environment(out) <- sys.frame(sys.nframe())
                           environment(out)$loadings=D@data$loadings
                           out}#, 
#                 irv = compute.irv, 
#                 irv.revised = compute.irv.revised
         )  
      out
      }
out
}