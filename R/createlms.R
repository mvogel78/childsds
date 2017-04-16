##' prepare data for repeated iteration process
##'
##' given a dataframe, the column name of the subject identifier, sex, age,
##' value and group colums, the function creates a dataframe containing only
##' these five columns with the standard column names group, subject, sex, age, value.
##' lines containing missing values are removed. 
##' @title prepare data for iteration process
##' @param data dataframe containing measurement values, age, sex, and subject identifier
##' @param group optional variable indicating groups of subjects within the data frame in most cases (families)
##' @param subject subject identifier
##' @param sex column containing the sex (or any other stratum), ideally of type character, iteration process will run on each of the levels separately
##' @param value numeric column containing the measurement values
##' @param age numeric column containing the age
##' @param lb optional - lower bound for age
##' @param ub optional - upper bound for age
##' @return list of dataframes containing the columns group, subject, sex, age, value; one dataframe for every level of sex
##' @author Mandy Vogel
##' @importFrom magrittr %<>%
##' @importFrom magrittr  %>%
##' @export
prepare_data <- function(data, group = NULL, subject = "SIC", sex = NULL, value = "value", age = "age", lb = -Inf, ub = Inf){
    n1 <- nrow(data)
    if(is.null(group)){
        data$group <- NA
        group <- "group"
    }
    if(is.null(sex)){
        print("No sex variable is given. Data will not be grouped.")
        data$sex <- "all"
        sex <- "sex"
    }    
    data %<>% dplyr::select_(group, subject, value, age, sex)
    data %<>% dplyr::rename_("subject" = subject,
                      "group" = group,
                      "sex" = sex,
                      "value" = value,
                      "age" = age) 
    data %<>% tidyr::drop_na(subject, sex, value, age) %>% dplyr::ungroup()
    data %<>% dplyr::filter(dplyr::between(age, lb, ub))
    split(data, data$sex)
}

##' Select groups (families)
##'
##' function selects a given proportion of groups/families from the data
##' if no grouping variable is given the original data set is returned
##' function is called inside \code{\link{do_iterations}} and may not called directly
##' @title select families
##' @param data dataframe as returned by prepare data
##' @param prop proportion of families to be sampled
##' @param group name of the group variable (character) if not "group", ignored
##' @return dataframe containing only prop.fam percent the families in data
##' @author Mandy Vogel
##' @export
select_fams <- function(data, prop = 0.75, group){
    if(sum(is.na(data$group)) > 0) {
        print("Missing group variable. Returning original data set.")
        return(data)}
    weights <- dplyr::group_by(data, group) %>% dplyr::summarise(n=dplyr::n(), wgt = 1-1/(dplyr::n()+1))
    weights <- weights$group[sample(1:nrow(weights),size = (nrow(weights) * prop), prob = weights$wgt)]
    data[data$group %in% weights,]
}


##' Choose one measurement per subject
##'
##' function samples one measurement per subject, if prop < 1 additional
##' a prop*100 percent will be sampled from the measurements
##' the function is called inside \code{\link{do_iterations}} and may not called directly
##' @title choose one measurement per subject
##' @param data dataframe as returned by prepare data
##' @param subject name of the column containing the subject identifier
##' @param prop optional - proportion of measurements to sample
##' @param verbose if TRUE information about sample size is printed out
##' @return dataframe containing the sampled rows
##' @author Mandy Vogel
##' @export
select_meas <- function(data, subject = "subject", prop = 1, verbose = F){
    n1 <- nrow(data)
    if(sum(duplicated(data$subject)) > 0 ) data %<>% dplyr::group_by(subject) %>% dplyr::sample_n(1)
    n2 <- nrow(data)
    if(prop < 1) data %<>% dplyr::ungroup() %>% dplyr::sample_frac(prop)
    n3 <- nrow(data)
    if(verbose) print(paste("sampled", n2, "lines from", n1, "lines. Choose final fraction of", prop,"makes" ,n3, "lines"))
    data
}

##' fit lms
##'
##' wrapper around the \code{\link[gamlss]{lms}} function in the gamlss package
##' returns the fitted lms-parameter at given age points
##' the function is called inside \code{\link{do_iterations}} and may not called directly
##' @title fit lms
##' @param data dataframe as return by select_meas()
##' @param age.min lower bound of age
##' @param age.max upper bound of age
##' @param age.int stepwidth of the age variable
##' @param dist distribution used for the fitting process, has to be one of BCCGo, BCPEo, BCTo as they are accepted by lms()
##' @param mu.df degree of freedem location parameter
##' @param sigma.df degree of freedem spread parameter
##' @param nu.df degree of freedem skewness parameter
##' @param tau.df degree of freedem kurtosis parameter
##' @param value names of the value variable (character) if different from value, ignored
##' @return list containing a dataframe of the fitted lms parameter at the given age points and the fitted model
##' @author Mandy Vogel
fit_gamlss <- function(data, age.min = 0.25, age.max = 18, age.int = 1/12, dist = "BCCGo",
                       mu.df = 4,sigma.df = 3, nu.df = 2, tau.df = 2, value){
    tr.obj <- try(mm <- lms(value, age, data = data[,-grep("group",names(data))],
                            families = dist,method.pb = "ML", k = 2,trace = F,
                            sigma.df = sigma.df, nu.df = nu.df, mu.df = mu.df, tau.df = tau.df))
    age <- seq(age.min, age.max, by = age.int)
    if (mm$family[1] == dist & !("try-error" %in% class(tr.obj))) {
        lms <- as.data.frame(gamlss::predictAll(mm,
                                        newdata = data.frame(age = age)))
        lms$age <- age
        lms %<>% dplyr::select(age, dplyr::everything())
        return(list(lms = lms, model = mm))
    }
    invisible(return(NULL))
}
##' one iteration
##'
##' function samples families then measurements and fits the model
##' the function is called inside \code{\link{do_iterations}} and may not called directly
##' @title one iteration
##' @param data.list list of dataframes as returned by prepare_data
##' @param prop.fam proportion of families to be sampled
##' @param prop.subject proportion of subject to be sampled
##' @inheritParams fit_gamlss
##' @return list of lists each containing a dataframe of the fitted lms parameter at the given age points and the fitted model
##' @author Mandy Vogel
##' @export
one_iteration <- function(data.list, prop.fam = 0.75, prop.subject = 1, age.min = 0, age.max = 18, age.int = 1/12,
                          dist = "BCCGo", sigma.df = 3, nu.df = 2, mu.df = 4, tau.df = 2){
    if(sum(is.na(data.list[[1]]$group)) > 0) print("no grouping variable is given. Therefore, no grouping will be done.")
    tmp.l <- lapply(data.list, select_fams, prop = prop.fam)
    tmp.l <- lapply(tmp.l, select_meas, prop = prop.subject)
    lapply(tmp.l, fit_gamlss, dist = dist, sigma.df = sigma.df, nu.df = nu.df, mu.df = mu.df, tau.df = tau.df )
}

##' Do lms iterations 
##'
##' function samples families, samples measurements (and subjects), fits the model for a
##' given number of iterations
##' @title do lms iterations    
##' @param n number of iterations
##' @inheritParams one_iteration
##' @return list of lists for models and fitted parameters
##' @author Mandy Vogel
##' @export
do_iterations <- function(data.list, n = 10, prop.fam = 0.75, prop.subject = 1, age.min = 0, age.max = 18, age.int = 1/12,
                          dist = "BCCGo", mu.df = 4, sigma.df = 3, nu.df = 2, tau.df = 2){
    sexes <- names(data.list)
    res <- list()
    for(i in 1:n){
        res[[length(res) + 1]] <- one_iteration(data.list = data.list,
                                                dist = dist,
                                                mu.df = mu.df,
                                                sigma.df = sigma.df,
                                                nu.df = nu.df,
                                                tau.df = tau.df,
                                                prop.fam = prop.fam)
    }
    lms <- lapply(sexes, function(sex) {
        lms <- lapply(res, function(x) x[[sex]]$lms)
        lapply(lms, function(ls) ls[sapply(ls, function(x) !is.null(x))])
    })
    names(lms) <- sexes
    print(paste("fitted out of", n, "iterations:"))
    print(sapply(lms, length))
    models <- lapply(sexes, function(sex) {
        models <- lapply(res, function(x) x[[sex]]$model)
        lapply(models, function(ls) ls[sapply(ls, function(x) !is.null(x))])
    })
    names(models) <- sexes
    attr(lms, "distribution") <- dist
    list(lms = lms, models = models)

}

##' aggregate lms parameters
##'
##' function takes the lms part of the result from the do_iterations() function and returns
##' the mean parameters
##' @title aggregate lms parameters
##' @param lms.list list of parameter tables as returned by do_iterations()
##' @return list of dataframes containing the aggregated parameters, each for every level of sex
##' @author Mandy Vogel
##' @export 
aggregate_lms <- function(lms.list){
    dist <- attr(lms.list, "distribution")
    columns <- names(lms.list[[1]][[1]])
    lms.agg <- lapply(lms.list, function(lms) {
        as.data.frame(sapply(columns, function(col) rowMeans(Reduce(cbind,lapply(lms, function(x) x[,col]))),
                             USE.NAMES = T))})
    attr(lms.agg, "distribution") <- dist
    lms.agg
}

