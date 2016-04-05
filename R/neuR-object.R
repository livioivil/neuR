#' An S4 class to store fNIRS, fMRI, EEG data
#'
#' @slot data list of 3D arrays
#' @slot mask 
#' @slot info 
#' @export


setClass("neuR.object", 
  representation(
    data = "list",
    mask = "array", 
    info = "list",
    functs = "list"
    ),
  prototype = list(
    data = list(),
    mask = array(), 
    info = list(),
    functs = list()
  )
)


#==========================================================
# Function "show" prints a "neuR.object" object
#==========================================================
setMethod("show", "neuR.object",
          function(object)
          {
            cat("
                A neuR-object with volumes of size:",dim(object),"(=",prod(dim(object)),"overall)")
            cat("
                It contains the following maps:
                ")
            if(length(object@data)>0){
              out=t(sapply(object@data,dim))
              colnames(out)=c("Volumes","Voxels","Blocks")
              cat("\n")
              print(out)
            } else
              cat("No maps!")
            
            cat("and the following functions:")
            if(length(object@functs)>0){ 
              cat(names(object@functs))
            } else
              cat("No functions!")
          }
          )

#==========================================================
# Function "summary" prints a "neuR.object" object
#==========================================================

setGeneric("summary")
setMethod("summary", "neuR.object", function(object)
{
  cat("
      A neuR-object with volumes of size:",dim(object),"(=",prod(dim(object)),"overall)")
  cat("
It contains the following maps:
      ")
  if(length(object@data)>0){
    out=t(sapply(object@data,dim))
    colnames(out)=c("Volumes","Voxels","Blocks")
    cat("\n")
    print(out)
  } else
    cat("No maps!")

  cat("and the following functions:")
  if(length(object@functs)>0){ 
    cat(names(object@functs))
  } else
    cat("No functions!")
})



# ==========================================================
# setGeneric("dim", function(object, ...) standardGeneric("dim"))
setMethod("names", "neuR.object",
          function(x) {
            names(x@data)
          }
)


# ==========================================================
# setGeneric("dim", function(object, ...) standardGeneric("dim"))
setMethod("dim", "neuR.object",
  function(x) {
    x@info$dim.vol
  }
)





#==========================================================
# The length method for "neuR.object"
#==========================================================
setMethod("length", "neuR.object", 
            function(x) 
{
  x@info$ntimes
})


