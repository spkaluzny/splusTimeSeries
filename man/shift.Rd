\name{shift}
\alias{shift}
\alias{shift,ANY-method}
\title{
  Create a Shifted Time Series
}
\description{
Returns a time series like the input but shifted in time.
}
\usage{
shift(x, k=1)
}
\arguments{
\item{x}{
a univariate or multivariate regular time series.
Missing values (\code{NA}s) are allowed.
}
\item{k}{
the number of positions the input series is to lead the new series.
That is, the resulting series is shifted forwards in time;
negative values lag the series backwards in time.
Non-integer values of \code{k} are rounded
to the nearest integer.
}
}
\value{
returns a time series with the same data as \code{x},
but with positions lagged by \code{k} steps.
}
\note{
The \code{shift} function replaces the \code{lag} function,
which illogically had the opposite sign of shifting.
(The \code{lag} function has been retained only because 
it is used in other functions.)
}
\details{
\code{shift} is a generic function.
Its default method calls \code{lag(x,-k).}

\code{shift} also has a method
for \code{series} objects,
which works for both \code{timeSeries}
and \code{signalSeries} objects.


\itemize{
\item To align the times of several new-style time series, 
  use \code{seriesMerge}.
\item To align the times of several old-style time series, use 
 \code{ts.intersect} or \code{ts.union}.
\item To compute a lagged/leading series with same time position 
  but shifted data slot, use \code{seriesLag}. (\code{seriesLag} 
  also works for both \code{timeSeries} and \code{signalSeries} objects.)
}


}
\seealso{
\code{\link{seriesMerge}},
\code{\link{lag}},
\code{\link{lag.plot}},
\code{\link{ts.intersect}},
\code{\link{ts.union}}.
}
\examples{
x <- signalSeries(data=data.frame(a=1:10, b=letters[1:10]), positions=1:10)
x5 <- shift(x,5)
seriesMerge(x, x5, pos="union")
}
\keyword{ts}
% docclass is function
% Converted by mySd2Rd version 47442.
