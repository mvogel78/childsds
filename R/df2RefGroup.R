##' convert data frame to RefGroup class
##' 
##' the first column of the dataframe should been the age,
##' the second column of the dataframe should been the gender,
##' and the female and male should been named by the `gender` parameter
##' 
##' @title convert data frame to RefGroup class
##' @param dat, dataframe whose first column is age and second column is gender
##' @param gender, 2-length vector used to name the female and male 
##' @param dist, character indicate the distribution and should define in the `gamlss.dist`
##' @author Congcong Gong
##' @return RefGroup
##' @importFrom magrittr  %>%
##' @importFrom tidyr gather
##' @importFrom tidyr drop_na
##' @importFrom dplyr group_by
##' @importFrom dplyr do
##' @importFrom psych skew
##' @export
df2RefGroup <- 
        function(dat, gender = c('female'='Female', 'male'='Male'),
                 dist="BCCG", citations = list(), info = list(),name=''){
                
                colnames(dat)[1:2] <- c('age','gender')
                if (!is.numeric(dat$age)){
                        stop("The first columns must be the age whose class is numeric")
                }
                
                dat$gender <- factor(dat$gender, levels = gender)
                if (all(is.na(dat$gender))){
                        stop("The second columns must be the gender, female & male should be called in a way setted by the parameter gender")
                }
                
                items <- vector('list',ncol(dat)-2)
                
                colnames(dat) <- tolower(colnames(dat))
                items_n <- colnames(dat)[-c(1,2)]
                
                #dat$age <- round(dat$age,digits = 2)
                summ <- function(d){
                        d %>% 
                                gather(value = 'value', key = 'key',-c(age, gender)) %>% 
                                group_by(age, gender, key) %>% 
                                do({
                                        nu = skew(.$value,na.rm=T)
                                        mu = mean(.$value,na.rm=T)
                                        sigma = sd(.$value,na.rm=T)
                                        data.frame(nu=nu,mu=mu,sigma=sigma)
                                })
                }
                dat <- summ(dat)
                items <- lapply(items_n, function(x) {
                        f <- dat[dat$key==x & dat$gender == gender['female'], 
                                 c('age', 'nu', 'mu', 'sigma')] %>% 
                                drop_na()
                        m <- dat[dat$key==x & dat$gender == gender['male'], 
                                 c('age', 'nu', 'mu', 'sigma')]%>% 
                                drop_na()
                        
                        new('ParTab', 
                            item = x,
                            dist = list(male = dist, female = dist),
                            params = list(male = m, female=f))
                }
                )
                names(items) <- items_n
                new("RefGroup",
                    name = name,
                    refs = items,
                    citations = citations,
                    info = info)
        }