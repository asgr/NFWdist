\name{nfw}
\alias{dnfw}
\alias{pnfw}
\alias{qnfw}
\alias{rnfw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Standard Distribution Functions for the 3D NFW Profile
}
\description{
Density, distribution function, quantile function and random generation for the 3D NFW profile
}
\usage{
dnfw(x, con = 5, log = FALSE)
pnfw(q, con = 5, log.p = FALSE)
qnfw(p, con = 5, log.p = FALSE)
rnfw(n, con = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,q}{
Vector of quantiles. This is scaled such that \option{x}=R/Rvir for NFW. This means the PDF is only defined between 0 and 1.
}
  \item{p}{
Vector of probabilities.
}
  \item{n}{
Number of observations. If length(n) > 1, the length is taken to be the number required.
}
  \item{con}{
The NFW profile concentration parameter, where c=Rvir/Rs.
}
  \item{log, log.p}{
Logical; if TRUE, probabilities/densities p are returned as log(p).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
dnfw gives the density, pnfw gives the distribution function, qnfw gives the quantile function, and rnfw generates random deviates.
}
\references{
None as of yet!
}
\author{
 Aaron Robotham
}
\note{
This seems to work at least as efficiently as accept reject, but it is ultimately much more elegant code.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Nothing to see here!
}
\examples{
for(con in c(1,5,10,20)){
  magplot(density(rnfw(1e6,con=con), bw=0.01))
  lines(seq(0,1,len=1e3), dnfw(seq(0,1,len=1e3),con=con),col='red')
  legend('topright',legend=paste('con =',con))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nfw }% use one of  RShowDoc("KEYWORDS")
