\name{ts.update}
\alias{ts.update}
\title{
  Update Old \code{ts} Objects 
}
\description{
Converts an old \code{ts} object to a \code{signalSeries} object. 
}
\usage{
ts.update(x)
}
\arguments{
\item{x}{
the time series to convert. 
}
}
\value{
returns a \code{signalSeries} object with equivalent positions.
}
\seealso{
\code{\linkS4class{signalSeries}}.
}
\examples{
ts.update(ts(1:10)) 
}
\keyword{ts}

