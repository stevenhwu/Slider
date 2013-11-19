\name{gelLRT}
\Rdversion{1.1}
\alias{gelLRT}

\title{
Calculate the Likelihood Ratio Statistics 
}
\description{
Calculate the Likelihood Ratio Statistics for each spot from the gel data
}
\usage{
gelLRT(allData, group, th = NA, tMax = log2(100))
}

\arguments{
  \item{allData}{
	Numeric n by g matrix records log intensities from gel data. Where n is the number of spots and g is the number of gels.
}
  \item{group}{
        List of length 2 which acts as the index of each group (case/control). Each element in the list records the index position of gels for each group.
}
 
  \item{th}{
	Numeric values for the limits of detection. Default value is the minimum value from gel data.
}
\item{tMax}{
        Theoretical maximum, default log2(100)
}
}
\details{
        The parameter "group" is a list of length 2. One of them records the index position for case group and the other one records the index poistion for the control group. The index refers to the index of column in the data matrix. 
        e.g. group=list(c(1,3,5),c(2,4,6)). One group contains gel 1,3 and 5, which corresponed to column 1,3 and 5 in the data matrix.
}
\value{
    Return a vector of length n which contain the likelihood statistics for each individual spot. 
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
S. Wu
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[slider:simGel]{simGel}}, \code{\link[stats:Chisquare]{pchisq}}, \code{\link[stats:p.adjust]{p.adjust}}
}
\examples{
data(simGel)
result<-gelLRT(simGel$data,simGel$group)
LRT.pvalue<-pchisq(result, df=2, lower.tail = FALSE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }

