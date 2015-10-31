# #==========================================================
# # add funct to neuR-object
# #==========================================================
# map = function to be returned

add.neuR.map <- function(obj,map,out.name=map,...){
  temp=get.neuR.map (obj,map,...)
  if(is.list(temp)) 
    obj@data[names(temp)]=temp else  
      obj@data[[out.name]]=temp
  obj
}