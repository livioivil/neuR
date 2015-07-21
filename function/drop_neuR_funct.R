# #==========================================================
# # drop maps of neuR-object
# #==========================================================
# map = function to be returned


drop.neuR.funct<- function(obj,funct){
  obj@functs[[funct]]=NULL
  obj
}