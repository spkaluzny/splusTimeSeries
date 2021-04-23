\name{timeSeries-class}
\alias{timeSeries-class}
\alias{deltat,timeSeries-method}
\alias{start,timeSeries-method}
\alias{coerce,timeSeries,data.frame-method}
\alias{as.data.frame,timeSeries-method}
\title{
  Calendar Time Series Class
}
\docType{class}
\description{
Represents calendar time series objects.
}
\section{Slots}{
All slots except the last two, \code{fiscal.year.start} and \code{type},
are inherited from the base \code{series} class.
\describe{
\item{data}{
(\code{ANY}) the variable data,
which can be any data object for which \code{is.rectangular} is \code{TRUE},
such as a \code{data.frame}, \code{matrix}, or atomic vector.
}
\item{positions}{
(\code{positions}) the x values for the variables,
which must be of type \code{positionsCalendar}.
}
\item{start.position}{
(\code{positions}) the starting x value.
}
\item{end.position}{
(\code{positions}) the ending x value.
}
\item{future.positions}{
(\code{positions}) future x values used for predictions.
}
\item{units}{
(\code{character}) the units for the data.
}
\item{title}{
(\code{character}) the title of the data set.
}
\item{documentation}{
(\code{character}) user-supplied documentation.
}
\item{attributes}{
(\code{ANY}) the attributes slot for arbitrary use.
}
\item{fiscal.year.start}{
(\code{numeric}) the month number for fiscal year start.
}
\item{type}{
(\code{character}) the type of time series.
}
}
}
\details{
The \code{timeSeries} class inherits \code{series} and \code{seriesVirtual}.
From \code{series}, it inherits slots that hold x positions and variable data.
A \code{timeSeries} object is valid only when the lengths of the positions and
data match, the \code{data} slot is rectangular,
and the \code{positions} slot holds a \code{positionsCalendar} object.
}
\section{Series functions}{
You can create objects of class \code{timeSeries} using the 
\code{new} function, in which case they are set up to be empty 
and have their fiscal year starting in January. Alternatively, you 
can create objects of class \code{timeSeries}  using the \code{timeSeries} 
function. 

These can be subscripted and used in mathematical operations much
like data frames or matrices.
}
\seealso{
  \code{\linkS4class{series}}  class, \code{\link{timeSeries}},
  \code{\link{is.rectangular}}.
}
\keyword{ts}
\keyword{classes}
