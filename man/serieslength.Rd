\name{seriesLength}
\alias{seriesLength}
\title{
Length of a \code{timeSeries}
}
\description{
Returns the length of a \code{timeSeries}; that is, it returns the number of positions
in the \code{timeSeries}.
}
\usage{
seriesLength(x)
}
\arguments{
  \item{x}{
an object of class \code{timeSeries}.
}
}
\value{
returns the length of the \code{timeSeries}.
}
\note{
  This function is distinquished from the \code{length} function, which returns
  the number of series in the \code{timeSeries} object.
}
\seealso{
\code{\linkS4class{timeSeries}}  class.
}
\examples{
x <- timeSeries(data=data.frame(x=1:10, y=11:20), from="7/4/2000", by="bizdays")
seriesLength(x)
length(x)
}
\keyword{ ts }
