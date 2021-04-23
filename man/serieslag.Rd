\name{seriesLag}
\alias{seriesLag}
\alias{seriesLag,series-method}
\title{
Time Series Lag/Lead Function
}
\description{
Returns a lagged/leading \code{timeSeries} or \code{signalSeries} object.
}
\usage{
seriesLag(X, k = 1, trim = FALSE, pad = NA)
}
\arguments{
  \item{X}{
    an object of class \code{timeSeries} or \code{signalSeries}.}
  \item{k}{
    the number of positions the new time series or signal series is
    to lag or lead the input series, with a positive value
    resulting in a lagged series and a negative value resulting in
    a leading series.
  }
  \item{trim}{
    a logical flag. If \code{TRUE}, the missing values at the beginning or
    end of the returned series will be trimmed. The default is
    \code{FALSE}.
  }
  \item{pad}{
    any padding to fill in the beginning or ending missing values.
    The default is \code{NA}.
  }
}
 \value{
   returns a lagged or leading time (signal) series of the original data.
}
\details{
The difference between \code{shift} and \code{seriesLag} is
that the returned series of \code{shift} is shifted in time (\code{position})
while the returned series of \code{seriesLag} shifts the entire data slot
but keeps the same time (\code{position}) intact.
They all work for both \code{timeSeries} and \code{signalSeries} objects.
}
\seealso{
  \code{\link{lag}}, \code{\link{shift}} .
  }
\examples{
x <- timeSeries(data=data.frame(x=1:10, y=11:20),
    from="7/4/2000", by="bizdays")
seriesLag(x, 1)
seriesLag(x, -1)
}
\keyword{ts}
