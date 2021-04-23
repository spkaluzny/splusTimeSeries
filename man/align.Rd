\name{align}
\alias{align}
\title{
  Time Series and Signal Interpolation and Alignment 
}
\description{
Aligns or interpolates a time series or signal to new positions. 
}
\usage{
align(x, pos, how="NA", error.how="NA", localzone=FALSE, matchtol=0, by, 
      k.by=1, week.align=NULL, holidays=timeDate())
}
\arguments{
\item{x}{
the object to be aligned or interpolated. 
}
\item{pos}{
the new positions to align or interpolate it to  
(either \code{pos} or \code{by} is required). 
}
\item{how}{
specifies how to treat unmatched positions. Must be one of the following: 
\tabular{ll}{
\code{"NA"} \tab Insert a row of \code{NA}. \cr  
\code{"drop"} \tab Drop that position entirely. \cr 
\code{"nearest"} \tab Use the row with the nearest position. \cr 
\code{"before"} \tab Use the data from the row whose position  
is just before the unmatched position. \cr 
\code{"after"} \tab Use the data from the row whose position is  
just after the unmatched position. \cr 
\code{"interp"} \tab Interpolate linearly between \code{"before"} and \code{"after"}. 
}
}
\item{error.how}{
specifies available actions when an out of bounds error occurs. (Such an error can occur 
when \code{how} is  \code{"before"}, \code{"after"}, or \code{"interp"}). Must be one of
the following: 
\tabular{ll}{
\code{"NA"} \tab Insert a row of \code{NA}. \cr 
\code{"drop"} \tab Drop that position entirely. \cr 
\code{"nearest"} \tab Use the row with the nearest position. 
}
}
\item{localzone}{
if T (\code{x} must be a calendar-based time series),
align by matching/interpolating with both \code{x} and 
\code{pos} in their local time zones, instead of with 
the absolute GMT times. 
}
\item{matchtol}{
the tolerance for matching positions. Positions that match within \code{matchtol} do 
not invoke the \code{how} argument methods. 
}
\item{by}{
if \code{pos} is missing and \code{by} is supplied for aligning a  
calendar-based time series, new positions are generated as a regular 
time/date sequence using one of the following:
\itemize{
\item \code{by}
\item \code{k.by}
\item \code{week.align}  
\item \code{holidays}
} 
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
These strings give the time units of intervals between values in  
the sequence. 
}
\item{k.by}{
a non-zero integer giving the width of the interval between consecutive 
values in the sequence in terms of the units given in \code{by}.  Ignored if 
\code{by} is not a character string or if \code{pos} is given. 
}
\item{week.align}{
if not \code{NULL}, and \code{by} is \code{"weeks"},  
you can supply a character 
string (or a number, 0 to 6, with 0 being Sunday)  
to specify a weekday to align to. The character string must 
be sufficient to make a unique case-insensitive match to the strings 
in \code{timeSeriesOptions("time.day.name")}.  Ignored if 
\code{by} is not a character string or if a \code{pos} is given. 
}
\item{holidays}{
the holidays for business day sequences. (Ignored if \code{by} is not 
a character string or if \code{pos} is given.)   
}
}
\value{
returns a new time series or a signal whose positions are the passed-in positions or 
positions generated from \code{by}, \code{k.by}, and so on, and whose  
rows are derived from \code{x} as specified in the arguments.
(Can be a subset if \code{how} or \code{error.how} is \code{"drop"}.)  
}
\details{
If either \code{x} or \code{pos} (or the generated sequence) has zero length,  
a zero-length series is returned. 
}
\seealso{
\code{\linkS4class{timeSeries}},  \code{\linkS4class{signalSeries}},  \code{\link{positions}},  \code{\link{seriesMerge}}.   
}
\examples{
a <- signalSeries(pos=1:10, data=data.frame(a = 11:20, b = 5 * (1:10))) 
align(a, c(.2, 3, 7.8, 12), how = "interp", error.how = "nearest") 
a <- timeSeries(pos=as(1:10, "timeDate"), 
		data=data.frame(a = 11:20, b = 5 * (1:10))) 
alpos <- as(c(.2, 3, 7.8, 12), "timeDate") 
alpos@time.zone <- "JST" 
positions(a)@time.zone <- "PST" 
align(a, alpos, matchtol = 1, localzone = TRUE) 
align(a, matchtol=1, localzone=TRUE, by="days", k.by=2) 
}
\keyword{ts}

