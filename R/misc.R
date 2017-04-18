##' Calculate raw values for percentile curve
##'
##' calculates quantile values for given RefGroup and given
##' percentiles
##' @title calculate raw values 
##' @param refs Refgroup object
##' @param item name of the measurement item
##' @param perc vector of percentiles to be calculated
##' @param stack wether or not the data should be stacked, stacked data
##' would most possibly be used in ggplot2
##' @param sex name of the sex variable (character) if different from sex, ignored
##' @param age name of the age variable (character) if different from age, ignored
##' @return data frame either with the different percentiles as columns
##' or, if stacked, as data frame with four columns: age, sex, variable, value
##' @author Mandy Vogel
##' @export
make_percentile_tab <- function(refs, item, perc = c(2.5,5,50,95,97.5), stack = F, sex, age){
    reftabs <- refs@refs[[item]]@params
    nam <- paste(sprintf("perc_%02d",floor(perc)),
                 gsub("0.","", perc-floor(perc)), sep = "_")
    dists <- unlist(refs@refs[[item]]@dist)
    perc <- perc/100
    sexes <- c(male = "male", female = "female")
    pertab <- lapply(sexes, function(sex){
        perc.values <- lapply(perc, function(p){
            eval(parse(text = paste0("gamlss::q",dists[sex],"(",p,",",
                                     paste(
                                         paste0(names(reftabs[[sex]])[-1],
                                                "=reftabs[[\"",sex,"\"]]$",
                                                names(reftabs[[sex]])[-1] ),
                                         collapse = ","),
                                     ")")))})
        names(perc.values) <- nam
        perc.values$age <- reftabs[[sex]]$age
        perc.values$sex <- sex
        as.data.frame(perc.values, stringsAsFactors = F)
    } 
    )
    res <- Reduce(rbind, pertab)
    if( requireNamespace("reshape2") & stack)
        return(reshape2::melt(res, id.vars = c("age","sex")))
    if( !requireNamespace("reshape2") & stack)
        print("For stacking the package reshape2 is required")
    return(dplyr::select(res, sex, age, dplyr::everything()))
    }



calc_confints <- function(lms.list, perc = c(2.5,5,50,95,97.5), level = 0.95, type = c("point")){
    dist <- "BCPE"
    nam <- paste(sprintf("perc_%02d",floor(perc)),
                 gsub("0.","", perc-floor(perc)), sep = "_") 
    perc <- perc/100
    sexes <- names(lms.list)
    lapply(sexes, function(sex){
        res.l <- list()
        for(i in 1:length(lms.list[[sex]])){
            sex.df <- lms.list[[sex]][[i]]
            perc.values <-  lapply(perc, function(p, sex.df){
                eval(parse(
                    text = paste0("gamlss.dist::q",dist,"(",p,",",
                                  paste(
                                      paste0(names(sex.df)[-which(names(sex.df) == "age")],
                                             "=sex.df$",
                                             names(sex.df)[-which(names(sex.df) == "age")]) ,
                                      collapse = ","),
                                  ")")))}, sex.df = sex.df)
            names(perc.values) <- nam
            perc.values$age <- sex.df$age
            perc.values$sex <- sex
            res.l[[length(res.l) + 1]] <- as.data.frame(perc.values, stringsAsFactors = F)                
        }
        confenv <- lapply(nam, function(perc){
            pm <- sapply(res.l, function(xx, perc){
                xx[,perc]
            }, perc = perc)
            pm <- as.data.frame(t(envelope(mat = t(pm))[[type]]))
            names(pm) <- c("upper","lower")
            pm
        })
        names(confenv) <- nam
        confenv
    })
}

 
