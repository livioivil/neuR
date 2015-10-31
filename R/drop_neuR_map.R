# #==========================================================
# # drop maps of neuR-object
# #==========================================================
# map = function to be returned


drop.neuR.map <- function(obj,map){
  obj@data[[map]]=NULL
  obj
}