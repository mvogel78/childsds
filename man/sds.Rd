% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdsvals.r
\name{sds}
\alias{sds}
\title{Calculate SDS Values}
\usage{
sds(value, age, sex, item, ref, type = "SDS", male = "male",
  female = "female")
}
\arguments{
\item{value}{vector of measurement values}

\item{age}{vector of age values}

\item{sex}{vector of sex}

\item{item}{name of the item e.g. "height"}

\item{ref}{RefGroup object}

\item{type}{"SDS" or "perc"}

\item{male}{coding of sex for male}

\item{female}{coding of sex for female}
}
\value{
vector containing SDS or percentile values
}
\description{
Calculate SDS values
}
\details{
The function takes a vector of measurement values, and of age and of sex
and a RefGroup object as arguments. It calculates the sds or percentile
values.
}
\examples{
anthro <- data.frame(age = c(11.61,12.49,9.5,10.42,8.42,10.75,9.57,10.48),
                     height = c(148.2,154.4,141.6,145.3,146,140.9,145.5,150),
                     sex = sample(c("male","female"), size = 8, replace = TRUE),
                     weight = c(69.5,72.65,47.3,51.6,45.6,48.9,53.5,58.5))
anthro$height_sds <- sds(anthro$height,
                         age = anthro$age,
                         sex = anthro$sex, male = "male", female = "female",
                         ref = kro.ref,
                         item = "height",
                         type = "SDS")

anthro$bmi <- anthro$weight/(anthro$height**2) * 10000
anthro$bmi_perc <- sds(anthro$bmi,
                       age = anthro$age,
                       sex = anthro$sex, male = "male", female = "female",
                       ref = kro.ref,
                       item = "bmi",
                       type = "perc")
data(who.ref)
x <- data.frame(height=c(50,100,60,54),
                sex=c("m","f","f","m"),
                age=c(0,2.9,0.6,0.2))
sds(value = x$height, age = x$age, sex = x$sex, male = "m", female = "f",
    ref = who.ref, item = "height")
}
\author{
Mandy Vogel
}
