\name{tcm.curve}
\alias{tcm.curve}
\docType{data}
\title{
  Treasury Constant Maturity Curve
}
\description{
  Treasury Constant Maturity Curve data
running from 1982 to 1997.  
The Constant Maturity Curve data come from yield curves
constructed by the U.S. Treasury Department from the
yields of actively traded issues adjusted to constant maturities.
}
\format{
\code{tcm.curve}  is a multivariate \code{timeSeries} object with the following columns:

\describe{

\item{three.month}{
Three-month rate.
}
\item{six.month}{
Six-month rate.
}
\item{one.year}{
One-year rate.
}
\item{two.year}{
Two-year rate.
}
\item{three.year}{
Three-year rate.
}
\item{five.year}{
Five-year rate.
}
\item{seven.year}{
Seven-year rate.
}
\item{ten.year}{
Ten-year rate.
}
\item{twenty.year}{
Constructed from the 20-year
Treasury department numbers, based on the 20-year bond through
December 1986 (at which time the 20-year bond was discontinued),
and from the new computation starting in October of 1993
based on outstanding bonds with approximately 20 years remaining
to maturity.  There is no data between 1987 and September 1992.
}
\item{thirty.year}{
Thirty-year rate.
}
\item{long.term}{
An unweighted average of rates on all outstanding bonds neither due nor callable
in less than 10 years, also calculated by the Treasury Department.
}
}
}
\source{
  From the web site of the
  Federal Reserve Bank,
  \url{http://www.federalreserve.gov/releases/h15/data.htm}.
}
\keyword{datasets}
