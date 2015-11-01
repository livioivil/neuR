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
                    pcBlocks = compute.pcs, 
                    irc = compute.irc, 
                    irh = compute.irh#, 
#                 irv = compute.irv, 
#                 irv.revised = compute.irv.revised
         )}
out
}