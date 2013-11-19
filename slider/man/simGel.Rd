\name{simGel}
\Rdversion{1.1}
\alias{simGel}
\docType{data}
\title{
Simulated dataset
}
\description{
Simulated data set where the difference between mean expression intensities is 1.5 standard deviation unit and probability of expression is 0.7 for both groups.
}
\usage{data(simGel)}
\format{
  
\tabular{lll}{

simGel$data    \tab     matrix  \tab Records the log expression intensities for 100 spots and 24 gels. Missing values are coded as NA \cr
simGel$group   \tab     list    \tab Lenght 2. Records which gels belong to which Case group, and which belong to control group.
}

}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
data(simGel)

simGel$group
## First 12 gels belong to Case group, others belong to Control Group
# $Case           
#  [1]  1  2  3  4  5  6  7  8  9 10 11 12
#
# $Control
# [1] 13 14 15 16 17 18 19 20 21 22 23 24

result<-gelLRT(simGel$data,simGel$group)
}
\keyword{datasets}
