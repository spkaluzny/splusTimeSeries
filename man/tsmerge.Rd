\name{seriesMerge}
\alias{seriesMerge}
\title{
  Merging for Time Series and Signals 
}
\description{
Merges time series or signal objects,  
making a new object with all the columns of  
the input objects, and some or all of the rows, depending on how their 
positions match. 
}
\usage{
seriesMerge(x1, x2, ..., pos=positions(x1), how,
        error.how, localzone=FALSE, matchtol=0,  
        suffixes)
}
\arguments{
\item{x1}{
the first object to be merged. 
}
\item{x2}{
the second object to be merged. 
}
\item{\dots}{
the other objects to be merged. 
}
\item{pos}{
the positions to align to, or \code{"union"} to make a union of all input positions. 
(The default argument values give an intersection of all the positions.) 
}
\item{how}{
after the positions to align to are determined, \code{how} determines  
how to treat positions that are missing from the various input objects. 

Can be one of the following:
\tabular{ll}{
\code{"NA"} \tab Inserts a row of \code{NA}. \cr  
\code{"drop"} \tab Drops that position entirely. \cr 
\code{"nearest"} \tab Uses the row with the nearest position. \cr 
\code{"before"} \tab Uses the data from the row whose position  
is just before the unmatched position. \cr
\code{"after"} \tab Uses the data from the row whose position is  
just after the unmatched position. \cr 
\code{"interp"} \tab Interpolates linearly between \code{"before"} and \code{"after"}.  
}
The default is \code{"drop"} unless \code{pos="union"}, 
in which case \code{"drop"} makes no sense and the default is \code{"NA"}. 
}
\item{error.how}{
specifies what to do in the event of an out of bounds error, which can 
occur when \code{how} is \code{"before"}, \code{"after"}, or \code{"interp"}. 

Can be one of the following: \tabular{ll}{
\code{"NA"} \tab Inserts a row of \code{NA} \cr 
\code{"drop"} \tab Drops that position entirely \cr
\code{"nearest"} \tab Uses the row with the nearest position. \cr
}
The default is \code{"drop"} unless \code{pos="union"}, 
in which case \code{"drop"} makes no sense and the default is \code{"NA"}. 
}
\item{localzone}{
if \code{TRUE} (that is, all input positions must be calendar-based),  
merge by matching/interpolating with all positions 
in their local time zones, instead of with the absolute GMT times. 
}
\item{matchtol}{
the tolerance for matching positions. Positions that match within \code{matchtol} do 
not invoke \code{how} argument methods. 
}
\item{suffixes}{
the suffixes to append to the column names that are duplicated between  
the various input data objects. The default value is  
\code{paste(".", 1:nargs, sep = "")}, 
where \code{nargs} is the total number of data arguments. 
}
}
\value{
returns a new \code{series} object containing all the columns of all the inputs, and  
all the rows of all the inputs, according to the alignment methods described above. 
}
\seealso{
\code{\linkS4class{timeSeries}},  \code{\linkS4class{signalSeries}},  \code{\link{positions}},  \code{\link{align}},  \code{\link{merge}}.   
}
\examples{
a <- signalSeries(pos=1:10, data=data.frame(a = 11:20, b = 5 * (1:10))) 
b <- signalSeries(pos=5:14, data=data.frame(a = 11:20, b = 5 * (1:10))) 
seriesMerge(a, b) 
a <- timeSeries(pos=as(1:10, "timeDate"), 
		data=data.frame(a = 11:20, b = 5 * (1:10))) 
b <- timeSeries(pos=as(5:14, "timeDate"), 
	        data=data.frame(a = 11:20, b = 5 * (1:10))) 
seriesMerge(a, b, pos="union") 
}
\keyword{ts}

