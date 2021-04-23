\name{seriesData}
\alias{seriesData}
\alias{asSeriesData}
\alias{seriesData<-}
\alias{seriesDataNew}
\alias{seriesDataValid}
\title{
  SeriesData of \code{series} Objects 
}
\description{
Accesses the seriesData of \code{series} objects. 
}
\usage{
seriesData(object)
asSeriesData(object)
seriesData(object) <- value
seriesDataNew()
seriesDataValid(object)
}
\arguments{
  \item{object}{
    the object for which to find \code{seriesData}. 
  }
  \item{value}{
    the value to which to set \code{seriesData}.
  }
}
\value{
returns the \code{seriesData} slot of \code{object}. 
}
\details{
This function can also be used on the left side of an assignment 
to set the \code{seriesData} of a \code{series} object. 
}
\seealso{
\code{\link{positions}},  \code{\linkS4class{timeSeries}},  \code{\linkS4class{signalSeries}}.   
}
\examples{
x <- signalSeries(pos=1:10, data=11:20) 
seriesData(x) 
seriesData(x) <- 1:10
}
\keyword{ts}
