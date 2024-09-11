\name{unionPositions}
\alias{unionPositions}
\title{
  Positions Object Union With Tolerance
}
\description{
Makes a union of numeric or calendar positions (that is, positions of
series objects (which can be numeric), time vectors, or sequences) 
objects using \code{localzone} and \code{matchtol} as in the \code{seriesMerge} and \code{align}
functions.
}
\usage{
unionPositions(..., localzone = FALSE, matchtol = 0)
}
\arguments{
\item{\dots}{
the positions objects to be joined.
}
\item{localzone}{
a logical value. If \code{TRUE}, creates a union by matching with all
passed-in positions in their local time zones, instead of with
the absolute GMT times. (The positions must be calendar-based.)
}
\item{matchtol}{
the tolerance for matching positions. Positions that match within \code{matchtol} are
not duplicated in the output.
}
}
\value{
Returns a new positions object containing all of the input positions,
with duplicates (as defined by \code{matchtol} and \code{localzone}) removed.
Returns \code{numeric(0)} if no \code{...} arguments are given.
}
\seealso{
\code{\link{positions}},
\code{\link{align}},
\code{\link{seriesMerge}}. 
}
\examples{
unionPositions(1:10, 5:20)
unionPositions(1:10, 5.1:20.1, matchtol=.3)
unionPositions(timeCalendar(d=1:10), timeCalendar(d=5:20))
unionPositions(timeCalendar(d=1:10, zone="PST"),
               timeCalendar(d=5:20, zone="EST"))
unionPositions(timeCalendar(d=1:10, zone="PST"), 
               timeCalendar(d=5:20, zone="EST"), localzone=TRUE)
}
\keyword{ts}
