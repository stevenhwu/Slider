\name{slider-package}
\Rdversion{1.5.2}
\alias{slider-package}
\alias{slider}
\docType{package}
\title{
Statistical Likelihood model for Identifying Differential Expression in R
}
\description{
Using a likilihood model to identify differentially expressed proteins in 2D PAGE and take into account proteion's  expression intensities fall below the limit of detection. 
}

\details{
\tabular{ll}{
Package: \tab slider\cr
Type: \tab Package\cr
Version: \tab 1.5.2\cr
Date: \tab 2011-08-10\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

Implemeted the liklihood model proposed by Wu et. al 
}
\author{
Steven Wu
}
\references{
Wu SH, Black MA, North RA, Atkinson KR, Rodrigo AG (2009) A Statistical Model to Identify Differentially Expressed Proteins in 2D PAGE Gels. PLoS Comput Biol 5(9): e1000509. doi:10.1371/journal.pcbi.1000509

}

\keyword{ package }
\seealso{

\code{\link[slider:gelLRT]{gelLRT}}
}
\examples{
data(simGel)
gelLRT(simGel$data,simGel$group)
}
