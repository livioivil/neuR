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
  str(object)
})





# ==========================================================
# setGeneric("dim", function(object, ...) standardGeneric("dim"))
setMethod("dim", "neuR.object",
  function(x) {
    dim(x@info$dim.vol)
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

