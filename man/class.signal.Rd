\name{signalSeries-class}
\alias{signalSeries-class}
\alias{start,signalSeries-method}
\alias{end,signalSeries-method}
\alias{coerce,signalSeries,data.frame-method}
\alias{as.data.frame,signalSeries-method}
\title{
  signalSeries Class
}
\docType{class}
\description{
Represents non-calendar time series and signal
objects.
}
\section{Slots}{
All slots except the last, \code{units.position}, 
come from the \code{series} object. 
\describe{
\item{data}{
(\code{ANY}) the variable data, which can be any data object for which 
\code{is.rectangular} is \code{TRUE},
such as a \code{data.frame}, \code{matrix}, or atomic vector. 
}
\item{positions}{
(\code{positions}) the x values for the variables, which must be 
of type \code{positionsNumeric}. 
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
\item{units.position}{
(\code{character}) the units for the \code{positions} slot. 
}
}
}
\details{
The \code{signalSeries} class inherits from the \code{series} 
and \code{seriesVirtual}classes. It has slots that hold x positions and variable data 
inherited from the \code{series} class. 

A \code{signalSeries} object is valid only when the lengths of the positions and data match, the data is a rectangular object, 
and the \code{positions} slot holds a \code{positionsNumeric} object. 
}
\section{signalSeries functions}{
You can create objects of class \code{signalSeries} using the \code{new} function, in 
which case they are set up to be empty. Alternatively, you can create objects 
of class \code{signalSeries} using the \code{signalSeries} function. 

These objects can be subscripted and used in mathematical operations much 
like data frames or matrices. 
}
\seealso{
\code{\linkS4class{series}} class, \code{\linkS4class{signalSeries}}, \code{\link{is.rectangular}}.
}
\keyword{ts}
\keyword{classes}
% docclass is function
% Converted by mySd2Rd version 47442.
