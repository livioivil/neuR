# #==========================================================
# # get functions to compute maps neuR-object
# #==========================================================
#' @param funct 
#' @param ... are used to set the parameters of the map 
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