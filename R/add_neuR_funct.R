# #==========================================================
# # add funct to neuR-object
# #==========================================================
# map = function to be returned

add.neuR.funct <- function(obj,funct,out.name=funct,...){
  obj@functs[[out.name]]=get.neuR.funct(funct,...)
  obj
}