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

##' Calculate SDS values
##'
##' The function takes a vector of measurement values, and of age and of sex
##' and a RefGroup object as arguments. It calculates the sds or percentile
##' values.
##' @title Calculate SDS Values
##' @param value vector of measurement values
##' @param age vector of age values
##' @param sex vector of sex 
##' @param item name of the item e.g. "height"
##' @param ref RefGroup object
##' @param type "SDS" or "perc"
##' @param male coding of sex for male
##' @param female coding of sex for male
##' @return vector containing SDS or percentile values
##' @author Mandy Vogel
sds <- function(value, age, sex, item, ref, type = "SDS", male = "male", female = "female"){
    if(!(length(value) == length(age) & length(value) == length(sex))){
        print("value, age, and sex must be of the same length")
        invisible(return(NULL))
    }
    sex <- as.character(factor(sex, levels = c(male, female), labels = c("male", "female")))
    refs <- ref@refs[[item]]@params
    dists <- ref@refs[[item]]@dist
    par.appr <- lapply(refs, function(df){
        as.data.frame(lapply(df, function(param) stats::approx(df$age, param, xout = age)$y))})
    res <- numeric()
    for(i in 1:length(value)) {
        res[i] <- eval(parse(text = paste0("stats::p",dists[[sex[i]]],"o(",value[i],",",
                                           paste(paste(names(par.appr[[sex[i]]])[-1],"=", par.appr[[sex[i]]][i,-1]), collapse = ","),
                                           ")")))
    }
    if(type == "SDS") return(stats::qnorm(res))
    return(round(res * 100, 2))
}


