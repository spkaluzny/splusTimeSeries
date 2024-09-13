\name{aggregateSeries}
\alias{aggregateSeries}
\title{
  Time Series and Signal Aggregation
}
\description{
Aggregation and coursening of time series and signals.
This is the method for the \code{aggregate} function for objects of class
\code{timeSeries} and \code{signalSeries}.
}
\usage{
aggregateSeries(x, pos, FUN, moving=FALSE, together=FALSE, drop.empty=TRUE,
                include.ends=FALSE, adj, offset, colnames, by, k.by=1,
                week.align=NULL, holidays=timeDate(), align.by=TRUE,
                incr=1, ...)
}
\arguments{
\item{x}{
the series object to be aggregated.
}
\item{pos}{
the break positions for aggregation (can also be supplied in the \code{by}
argument if desired).
}
\item{FUN}{
the function to use for aggregation. Often \code{mean}, \code{sum}, or \code{hloc}.
If \code{moving} is FALSE,
\code{FUN} can also be a character string like \code{"fastFFF"},
to aggregate column-wise using the corresponding fast 
\code{igroupFFF} function. (The \code{together} argument is ignored.)

Possible choices for \code{FFF} are currently
\code{Means}, \code{Maxs}, \code{Mins}, \code{Sums}, \code{Prods},
\code{Anys}, and \code{Alls}.
}
\item{moving}{
either \code{FALSE} to do standard aggregation,
or a positive integer \code{N} to perform a moving aggregation
(normally used for a moving average) over \code{N} samples.
}
\item{together}{
a logical value. if \code{TRUE}, passes all columns of \code{x} together into \code{FUN};
If \code{FALSE} (the default), passes each column separately into \code{FUN}
for each aggregation block.
}
\item{drop.empty}{
a logical value. If \code{TRUE} (the default), drops aggregation blocks with
no positions to aggregate from the output.
}
\item{include.ends}{
a logical value. If \code{TRUE}, includes the positions before the first
given aggregation block and after the last in the first/last blocks. 
If \code{FALSE} (the default), does not include those positions in the output.
}
\item{adj}{
if provided, adjusts the positions of the output series so that they lie a
fraction \code{adj} towards the blocks ending position. The default is to use
the lower end of the block for the output position.  0.5 uses the
center of the aggregation block for the output position, and 1.0 
uses the upper end of the block.
}
\item{offset}{
as an alternative to \code{adj}, provide a constant offset to add
to the lower end of the aggregation block to get the output series
positions.  For instance, in monthly aggregation, you might supply
an offset of 9 days so that the output positions fell on the tenth of
each month.
}
\item{colnames}{
new column names for the output series.  The default is to use the same
names as the input series if the output series has the same width.
}
\item{by}{
if \code{pos} is missing and \code{by} is supplied for aggregating
calendar-based time series, new positions are generated as a regular
time/date sequence using \code{by}, \code{k.by}, \code{week.align},
and \code{holidays}.

\code{by} gives the spacing between successive values in the sequence.
This can be a \code{timeSpan}, \code{timeRelative}, or \code{numeric} value,
in which case \code{k.by} is ignored.

Alternatively, it can be one of the following character strings:
\itemize{
\item \code{"milliseconds"}
\item \code{"seconds"}
\item \code{"minutes"}
\item \code{"hours"}
\item \code{"days"}
\item \code{"weekdays"}
\item \code{"bizdays"}
\item \code{"weeks"}
\item \code{"months"}
\item \code{"quarters"}
\item \code{"years"}
}
giving the time units of intervals between values in
the sequence.
}
\item{k.by}{
a non-zero integer giving the width of the interval between consecutive
values in the sequence in terms of the units given in \code{by}.  Ignored if
\code{by} is not a character string or if \code{pos} is given.
}
\item{week.align}{
if not \code{NULL} and \code{by} is \code{"weeks"},
you can supply a character
string (or a number, 0 to 6 with 0 being Sunday)
to specify a weekday to use for aggregation.  The character string must
be sufficient to make a unique case-insensitive match to the strings
in \code{timeSeriesOptions("time.day.name")}.  Ignored if
\code{by} is not a character string or \code{pos} is given.
}
\item{holidays}{
holidays for business day sequences. (Ignored if \code{by} is not
a character string or \code{pos} is given.)
}
\item{align.by}{
a logical value. If \code{TRUE} (the default), adjusted the sequence so that each element is on a whole number
of the \code{by * k.by} units. For example, if the units are 2 months, make the sequence
be only on the first of January, March, and so on.  Ignored if
\code{by} is not a character string. 
}
\item{incr}{
For moving aggregation, the moving window moves forward by this
many steps in the time series, for each window.
}
\item{\dots}{
Additional arguments to pass to FUN.
}
}
\value{
returns a new time series whose positions are the adjusted passed-in positions or
positions generated from \code{by}, \code{k.by}, and so on, 
(or possibly a subset if \code{drop.empty} is \code{TRUE}) and whose
rows are aggregated from \code{x} as specified in the arguments.  
Aggregation takes place by separating \code{x} into blocks separated by the positions
(or overlapping blocks with a fixed number of samples if \code{moving} is
supplied), and then applying \code{FUN} to each column (or all columns
together if \code{together} is \code{TRUE}) and forming a new time series with
the positions and aggregated data.
}
\seealso{
\code{\linkS4class{timeSeries}}, \code{\linkS4class{signalSeries}},
\code{\link{align}}, \code{\link{aggregate}}
}
\examples{
x <- timeSeries(data.frame(1:20,rnorm(20)), timeCalendar(d=1:20))
aggregate(x, FUN=mean, by="weeks")
}
\keyword{ts}
