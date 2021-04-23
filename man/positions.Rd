\name{positions}
\alias{positions}
\alias{positions<-}
\alias{positions<-,timeSeries-method}
\title{
  Positions of \code{series} Objects 
}
\description{
Accesses the positions of \code{series} objects. 
}
\usage{
positions(object)
positions(object) <- value
}
\arguments{
  \item{object}{
    the object for which to find positions. 
  }
  \item{value}{
    the value to which to set the positions.
    }
}
\value{
returns the \code{positions} slot of \code{object}. 
}
\details{
This function can also be used on the left side of an assignment 
to set the positions of a \code{series} object. 
}
\seealso{
\code{\link{seriesData}},  \code{\linkS4class{timeSeries}},  \code{\linkS4class{signalSeries}}.   
}
\examples{
x <- signalSeries(pos=1:10, data=11:20) 
positions(x) 
positions(x) <- 11:20 
}
\keyword{ts}
