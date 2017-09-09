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
##' @param female coding of sex for female
##' @return vector containing SDS or percentile values
##' @examples
##' anthro <- data.frame(age = c(11.61,12.49,9.5,10.42,8.42,10.75,9.57,10.48),
##'                      height = c(148.2,154.4,141.6,145.3,146,140.9,145.5,150),
##'                      sex = sample(c("male","female"), size = 8, replace = TRUE),
##'                      weight = c(69.5,72.65,47.3,51.6,45.6,48.9,53.5,58.5))
##' anthro$height_sds <- sds(anthro$height,
##'                          age = anthro$age,
##'                          sex = anthro$sex, male = "male", female = "female",
##'                          ref = kro.ref,
##'                          item = "height",
##'                          type = "SDS")
##' 
##' anthro$bmi <- anthro$weight/(anthro$height**2) * 10000
##' anthro$bmi_perc <- sds(anthro$bmi,
##'                        age = anthro$age,
##'                        sex = anthro$sex, male = "male", female = "female",
##'                        ref = kro.ref,
##'                        item = "bmi",
##'                        type = "perc")
##' data(who.ref)
##' x <- data.frame(height=c(50,100,60,54),
##'                 sex=c("m","f","f","m"),
##'                 age=c(0,2.9,0.6,0.2))
##' sds(value = x$height, age = x$age, sex = x$sex, male = "m", female = "f",
##'     ref = who.ref, item = "height")
##' @author Mandy Vogel
##' @export
sds <- function(value, age, sex, item, ref, type = "SDS", male = "male", female = "female"){
    if(!(length(value) == length(age) & length(value) == length(sex))){
        print("value, age, and sex must be of the same length")
        invisible(return(NULL))
    }
    if(!item %in% names(ref@refs)){
        stop(paste(item,"is not available in the given refs object.",
                    "Please choose one of the following items:",
                    paste(names(ref@refs), collapse = " - ")))
        return(invisible(NULL))
    }
    sex <- as.character(factor(sex, levels = c(male, female), labels = c("male", "female")))
    refs <- ref@refs[[item]]@params
    dists <- ref@refs[[item]]@dist
    par.appr <- lapply(refs, function(df){
        as.data.frame(lapply(df, function(param) stats::approx(df$age, param, xout = age, rule = 1)$y))})
    res <- numeric()
    for(i in 1:length(value)) {
        if(is.na(value[i]) | any(is.na(unlist(par.appr[[sex[i]]][i,-1]))) ){
            res[i] <- NA
        } else{
        res[i] <- eval(parse(text = paste0("gamlss.dist::p",dists[[sex[i]]],"(",value[i],",",
                                           paste(paste(names(par.appr[[sex[i]]])[-1],"=", par.appr[[sex[i]]][i,-1]), collapse = ","),
                                           ")")))
        }
    }
    if(type == "SDS") return(stats::qnorm(res))
    return(round(res * 100, 2))
}


##' Calculate SDS values for 2-dimensional matrix of covariates 
##'
##' The function takes a vector of measurement values, and of age and a
##' second covariate (like age and height for blood pressure) of sex
##' and a RefGroup object as arguments. It calculates the sds or percentile
##' values. This function is beta.
##' @title Calculate SDS Values for 2-dimensional matrix of covariates
##' @param value vector of measurement values
##' @param age vector of age values
##' @param x2 second vector of covariates
##' @param sex vector of sex 
##' @param item name of the item e.g. "height"
##' @param ref RefGroup object
##' @param type "SDS" or "perc"
##' @param male coding of sex for male
##' @param female coding of sex for male
##' @details the function searches for the nearest given point in the reference grid.
##' From there, the SDS/percentile value will be calculated. Different from \code{\link{sds}},
##' no interpolation will be applied. The procedure is according to Neuhauser et al. Blood
##' Pressure Percentiles by Age and Height from Nonoverweight Children and Adolescents
##' in Germany. 2011.
##' @return vector containing SDS or percentile values
##' @author Mandy Vogel
##' @export
sds_2d <- function(value, age, x2, sex, item, ref, type = "SDS", male = "male", female = "female"){
    if(!(length(value) == length(age) &
         length(value) == length(sex) &
         length(value) == length(x2))){
        print("value, age, x2, and sex must be of the same length")
        invisible(return(NULL))
    }
    if(!item %in% names(ref@refs)){
        stop(paste(item,"is not available in the given refs object.",
                    "Please choose one of the following items:",
                    paste(names(ref@refs), collapse = " - ")))
        return(invisible(NULL))
    }
    sex <- as.character(factor(sex,
                               levels = c(male, female),
                               labels = c("male", "female")))
    refs <- ref@refs[[item]]@params
    dists <- ref@refs[[item]]@dist
    par.appr <- list()
    par.appr <- lapply(refs, function(df){
        tmpdf <- data.frame(
                   mu = df$mu[class::knn(df[,c("age","x2")],
                                         data.frame(age = age,
                                                    x2 = x2),
                                         k = 1,cl = 1:nrow(df))],
                   nu = df$nu[class::knn(df[,c("age","x2")],
                                         data.frame(age = age,
                                                    x2 = x2),
                                         k = 1,cl = 1:nrow(df))],
                   sigma = df$sigma[class::knn(df[,c("age","x2")],
                                               data.frame(age = age,
                                                          x2 = x2),
                                               k = 1,cl = 1:nrow(df))]
                   ) 
        tmpdf
    })
    res <- numeric()
    for(i in 1:length(value)) {
        if(is.na(value[i]) | any(is.na(unlist(par.appr[[sex[i]]][i,]))) ){
            res[i] <- NA
        } else{
            print(paste0("gamlss.dist::p",dists[[sex[i]]],"(",value[i],",",
                                           paste(paste(names(par.appr[[sex[i]]]),"=", par.appr[[sex[i]]][i,]), collapse = ","),
                                           ")"))
        res[i] <- eval(parse(text = paste0("gamlss.dist::p",dists[[sex[i]]],"(",value[i],",",
                                           paste(paste(names(par.appr[[sex[i]]]),"=", par.appr[[sex[i]]][i,]), collapse = ","),
                                           ")")))
        }
    }
    if(type == "SDS") return(stats::qnorm(res))
    return(round(res * 100, 2)) 
}
