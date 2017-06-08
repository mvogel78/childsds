##' Calculate raw values for percentile curve
##'
##' calculates quantile values for given RefGroup and given
##' percentiles
##' @title calculate raw values 
##' @param ref Refgroup object
##' @param item name of the measurement item
##' @param perc vector of percentiles to be calculated
##' @param stack wether or not the data should be stacked, stacked data
##' would most possibly be used in ggplot2
##' @param sex name of the sex variable (character) if different from sex, ignored
##' @param age name of the age variable (character) if different from age, ignored
##' @return data frame either with the different percentiles as columns
##' or, if stacked, as data frame with four columns: age, sex, variable, value
##' @author Mandy Vogel
##' @examples
##' ptab <- make_percentile_tab(ref = kro.ref,
##'                             item = "height",
##'                            perc = c(2.5,10,50,90,97.5),
##'                            stack = TRUE)
##'
##' ggplot2::ggplot(ptab, ggplot2::aes(x = age, y = value, colour = variable)) +
##'    ggplot2::geom_line() +
##'    ggplot2::facet_wrap(~ sex, nrow = 2)
##' @export
make_percentile_tab <- function (ref, item, perc = c(2.5, 5, 50, 95, 97.5), stack = F,
                                 age = NULL, sex ) {
    reftabs <- ref@refs[[item]]@params
    if(is.null(age)) age <- ref@refs[[item]]@params[[1]]$age
    res <- list()
    for(df in reftabs){
        df2 <- as.data.frame(lapply(df[,-which(names(df)=="age")], 
                                    function(col){
                                        stats::approx(x = df$age, y =  col, xout = age, rule = 2)$y 
                                    }))
        df2$age <- age
        df2 <- dplyr::select(df2, age, dplyr::everything())
        res[[length(res)+1]] <- df2
    }
    names(res) <- names(reftabs)
    reftabs <- res 
    nam <- paste(sprintf("perc_%02d", floor(perc)),
                 gsub("0.", "", perc - floor(perc)), sep = "_")
    dists <- unlist(ref@refs[[item]]@dist)
    perc <- perc/100
    sexes <- c(male = "male", female = "female")
    pertab <- lapply(sexes, function(sex) {
        perc.values <- lapply(perc, function(p) {
            eval(parse(text = paste0("gamlss.dist::q", dists[sex], 
                                     "(", p, ",", paste(paste0(names(reftabs[[sex]])[-1], 
                                                               "=reftabs[[\"", sex, "\"]]$", names(reftabs[[sex]])[-1]), 
                                                        collapse = ","), ")")))
        })
        names(perc.values) <- nam
        perc.values$age <- reftabs[[sex]]$age
        perc.values$sex <- sex
        perc.values <- as.data.frame(perc.values, stringsAsFactors = F)
        perc.values <- dplyr::bind_cols(perc.values, reftabs[[sex]][-1])
        perc.values
    })
    res <- Reduce(rbind, pertab)
    if (requireNamespace("reshape2") & stack){
        res <- dplyr::select(res, -dplyr::matches("^mu|^nu|^sigma|^tau"))
        return(reshape2::melt(res, id.vars = c("age", "sex")))
    } 
        
    if (!requireNamespace("reshape2") & stack) 
        print("For stacking the package reshape2 is required")
    return(dplyr::select(res, sex, age, dplyr::everything()))
}

