.onLoad <- function(libname = find.package("childsds"), pkgname = "childsds"){
    if(getRversion() >= "3.1.0") utils::globalVariables(".data")
}
