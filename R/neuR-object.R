#' An S4 class to store fNIRS, fMRI, EEG data
#'
#' @slot data list of 3D arrays
#' @slot mask 
#' @slot info 
#'


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
# Function "summary" prints a "neuR.object" object
#==========================================================

setGeneric("summary")
setMethod("summary", "neuR.object", function(object)
{
  cat("A neuR-object with volumes of size:",dim(object),"(=",prod(dim(object)),"overall)")
  cat("
It contains the following maps:")
  if(length(object@data)>0){ 
    print(t(sapply(object@data,dim)))
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


