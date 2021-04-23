\name{hloc}
\alias{hloc}
\title{
  High, Low, Open, and Close Calculation 
}
\description{
Calculates the high, low, first, and last elements of a vector. 
Especially useful for financial trading data in conjunction with 
the \code{aggregateSeries} function. 
}
\usage{
hloc(x)
}
\arguments{
\item{x}{
a vector for which to calculate high, low, open, and close. 
}
}
\value{
returns a vector with four elements: 
\item{}{the maximum value in \code{x}.}
\item{}{the minimum value in \code{x}.}
\item{}{the first value of \code{x}.}
\item{}{the last value of \code{x}.}  

\code{x} can be an array, but dimensions are ignored.  
}
\seealso{
\code{\link{aggregateSeries}}. 
}
\examples{
x <- c(5, 2, 3, 6, 3, 2, 1, 7, 1) 
hloc(x) 
}
\keyword{ts}
% docclass is function
% Converted by mySd2Rd version 47442.
