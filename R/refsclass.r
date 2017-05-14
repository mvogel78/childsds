### Class RefGroup
##' Class of references
##'
##' Container for reference tables 
##'
##' @slot name name of the reference group
##' @slot refs List of references, each reference refers to one item and contains
##' independent variable age, and the parameter values for both genders
##' @slot citations information about the sources of the references
##' @slot info additional infos regarding the references
##' @exportClass RefGroup
##' @author Mandy Vogel
##' @examples
##' data(kiggs.ref)
##' print(kiggs.ref)
##' data(ukwho.ref)
##' print(ukwho.ref)
##' data(who.ref)
##' print(who.ref)
setClass(
    Class = "RefGroup",
    slots=list(
        name = "character",
        refs = "list",
        citations = "list",
        info = "list"
    )
)


##' show method for RefGroup
##'
##' show method for RefGroup
##' @title class RefGroup
##' @param object object of class RefGroup
##' @return prints information about age range, citations, etc.
##' @importFrom methods show
##' @author Mandy Vogel
setMethod("show","RefGroup",
          function(object){
              cat("*** Group of Reference Tables ***\n")
              print(paste(object@name, "containing", length(object@refs), "reference tables"))
              lapply(object@refs, show);
              lapply(object@citations, show);
              lapply(object@info, print)
              print(paste("use one of the following keys:",
                          paste(names(object@refs),
                                collapse = " - ")))
              
          })

### Class ParTab
##' Table of references
##'
##' Reference tables 
##'
##' @slot item identifier of the item
##' @slot dist named list which contains the distribution which was used in fitting 
##' the references. One entry for male and one for female
setClass(
  Class = "ParTab",
  slots=list(
      item = "character",
      dist = "list",
      params = "list"
    )
  )


##' show method for ParTab
##'
##' show method for ParTab
##' @title class ParTab
##' @param object object of calss ParTab
##' @return print information about the respective reference table
##' @author Mandy Vogel
##' @export
setMethod("show","ParTab",
          function(object){
            cat("\n*** Table of Reference Values ***\n")
            print(paste(object@item, "fitted with:",
                        paste(paste(names(object@dist), object@dist), collapse = ", ")))
            print(data.frame(
                sex = names(object@params),
                minage=sapply(object@params, function(x) min(x$age)),
                maxage=sapply(object@params, function(x) max(x$age)))) 
          })

