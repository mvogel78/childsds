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
##' @param age desired values of age 
##' @param include.pars indicator whether or not parameters should be included
##' @param digits specification of number of decimal places
##' @param sex name of the sex variable (character) if different from sex, not
##' functional in this version and therefore ignored
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
                                 age = NULL, include.pars = T, digits = 4, sex ) {
    if(stack) include.pars <- F 
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
            round(eval(parse(text = paste0("gamlss.dist::q", dists[sex], 
                                     "(", p, ",", paste(paste0(names(reftabs[[sex]])[-1], 
                                                               "=reftabs[[\"", sex, "\"]]$", names(reftabs[[sex]])[-1]), 
                                                        collapse = ","), ")"))),digits)
        })
        names(perc.values) <- nam
        perc.values$age <- reftabs[[sex]]$age
        perc.values$sex <- sex
        perc.values <- as.data.frame(perc.values, stringsAsFactors = F)
        if(include.pars)
            perc.values <- dplyr::bind_cols(perc.values, round(reftabs[[sex]][-1], digits))
        perc.values
    })
    res <- Reduce(rbind, pertab)
    if (requireNamespace("reshape2") & stack){
        return(reshape2::melt(res, id.vars = c("age", "sex")))
    } 
        
    if (!requireNamespace("reshape2") & stack) 
        print("For stacking the package reshape2 is required")
    return(dplyr::select(res, sex, age, dplyr::everything()))
}


##' Worm plot ggplot version
##'
##' creates a wormplot for a gamlss model or a given vector of
##' normalized quantile residuals, either for all residuals or
##' grouped by age intervals
##' @title Worm Plot ggplot version
##' @param m a gamlss model
##' @param residuals normalized quantile residuals
##' @param age numeric vector of ages
##' @param n.inter number of age intervals or cut points
##' @param y.limits limits of the y-axis
##' @return ggplot object
##' @export 
wormplot_gg <- function(m=NULL, residuals=NULL, age=NA, n.inter=1, y.limits = c(-1,1)){
    if(inherits(m,"gamlss"))
        residuals <- residuals(m)
    mm <- tibble::tibble(x = seq(-4,4,l = 1000),
                         yu = 1.96 * sqrt(stats::pnorm(.data$x)*(1-stats::pnorm(.data$x))/length(residuals))/stats::dnorm(.data$x),
                         yl = -.data$yu)
    mm <- reshape2::melt(mm, id.var = "x")
    tmp <- data.frame(residuals = residuals,
                      age = age)
    if(n.inter > 1) {
        if(all(is.na(age))) stop("intervals only possible of a vector of ages  is given")
        tmp$ag <- cut(tmp$age, n.inter)
    } else {
        tmp$ag <- "all ages"

    }
    tmp <- dplyr::group_by(tmp, .data$ag) %>% tidyr::nest()
    tmp <- dplyr::mutate(tmp, qq = purrr::map(.data$data, function(x){
        qq <- as.data.frame(stats::qqnorm(x$residuals, plot.it = F))
        qq$y <- qq$y - qq$x
        qq
    }))
    tmp <- tidyr::unnest(tmp, .data$qq)
    ggplot2::ggplot(tmp, ggplot2::aes_string(x = "x", y = "y")) +
        ggplot2::geom_point(shape = 21, size = 1, colour = "#2171B5") +
        ggplot2::geom_line(data = mm, inherit.aes = F,
                  ggplot2::aes_string(x = "x", y = "value", group = "variable"),
                  linetype = 2, colour = "forestgreen") +
        ggplot2::geom_vline(xintercept = 0, colour = "firebrick3", linetype = 4) +
        ggplot2::scale_y_continuous(limits = y.limits) +
        ggplot2::facet_wrap( ~ag) +
        ggplot2::labs(x = "Unit normal quantiles",
             y = "Deviation") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none",
              axis.title = ggplot2::element_text(size = 13, colour ="black"),
              axis.text = ggplot2::element_text(size = 13, colour ="black"),
              title = ggplot2::element_text(colour = "black"))
}
