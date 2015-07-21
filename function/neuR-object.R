#==========================================================
# CLASS DEFINITION *** CLASS DEFINITION *** CLASS DEFINITION
#==========================================================

# setClassUnion("arrayOrNULL", c("array", "NULL"))
# setClassUnion("listOrNULL", c("list", "NULL"))


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
    dim(x@mask)
  }
)





#==========================================================
# The length method for "neuR.object"
#==========================================================
setMethod("length", "neuR.object", 
            function(x) 
{
  nrow(x@data$tcs)
})

