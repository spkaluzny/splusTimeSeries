\name{series-class}
\alias{series-class}
\alias{seriesVirtual-class}
\alias{Math2,seriesVirtual-method}
\alias{Math,seriesVirtual-method}
\alias{Ops,ANY,seriesVirtual-method}
\alias{Ops,seriesVirtual,ANY-method}
\alias{Ops,seriesVirtual,missing-method}
\alias{Ops,seriesVirtual,seriesVirtual-method}
\alias{Summary,seriesVirtual-method}
\alias{[,seriesVirtual-method}
\alias{[<-,seriesVirtual,ANY,ANY,seriesVirtual-method}
\alias{[<-,seriesVirtual,ANY,ANY,vector-method}
\alias{aggregate,seriesVirtual-method}
\alias{boxplot,seriesVirtual-method}
\alias{c,seriesVirtual-method}
\alias{coerce,seriesVirtual,character-method}
\alias{coerce,seriesVirtual,complex-method}
\alias{coerce,seriesVirtual,integer-method}
\alias{coerce,seriesVirtual,logical-method}
\alias{coerce,seriesVirtual,matrix-method}
\alias{coerce,seriesVirtual,numeric-method}
\alias{coerce,seriesVirtual,vector-method}
\alias{colIds,seriesVirtual-method}
\alias{colIds<-,seriesVirtual-method}
\alias{cor,ANY,seriesVirtual-method}
\alias{cor,seriesVirtual,ANY-method}
\alias{cor,seriesVirtual,seriesVirtual-method}
\alias{deltat,seriesVirtual-method}
\alias{diff,seriesVirtual-method}
\alias{dim,seriesVirtual-method}
\alias{dimnames,seriesVirtual-method}
\alias{dimnames<-,seriesVirtual-method}
\alias{duplicated,seriesVirtual-method}
\alias{end,seriesVirtual-method}
\alias{start,seriesVirtual-method}
\alias{format,seriesVirtual-method}
\alias{frequency,seriesVirtual-method}
\alias{hist,seriesVirtual-method}
\alias{is.na,seriesVirtual-method}
\alias{length,seriesVirtual-method}
\alias{logb,seriesVirtual-method}
\alias{mean,seriesVirtual-method}
\alias{median,seriesVirtual-method}
\alias{names,seriesVirtual-method}
\alias{names<-,seriesVirtual-method}
\alias{ncol,seriesVirtual-method}
\alias{nrow,seriesVirtual-method}
\alias{numCols,seriesVirtual-method}
\alias{numRows,seriesVirtual-method}
\alias{numCols<-,seriesVirtual-method}
\alias{numRows<-,seriesVirtual-method}
\alias{pairs,seriesVirtual-method}
\alias{quantile,seriesVirtual-method}
\alias{rep,seriesVirtual-method}
\alias{rowIds,seriesVirtual-method}
\alias{rowIds<-,seriesVirtual-method}
\alias{row.names,seriesVirtual-method}
\alias{row.names<-,seriesVirtual-method}
\alias{seriesLength,seriesVirtual-method}
\alias{shift,seriesVirtual-method}
\alias{show,seriesVirtual-method}
\alias{sort.list,seriesVirtual-method}
\alias{sort,seriesVirtual-method}
\alias{subscript2d,seriesVirtual-method}
\alias{subscript2d<-,seriesVirtual-method}
\alias{summary,seriesVirtual-method}
\alias{window,seriesVirtual-method}
\alias{units,seriesVirtual-method}
\alias{var,seriesVirtual,ANY-method}
%
\title{
  Base Class for Time Series and Signals 
}
\docType{class}
\description{
A base class representing ordered data objects, 
such as time series and signals, that have 
positions (x values, times), and for each position a set of variables  
(stored in any rectangular data object).  
}
\section{Slots}{
\describe{
\item{data}{
(\code{ANY}) the variable data, which can be any data object for which 
\code{is.rectangular} is \code{TRUE},
such as a \code{data.frame}, \code{matrix}, or atomic vector. 
}
\item{positions}{
(\code{positions}) the x values for the variables. 
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
(\code{character}) units for the data. 
}
\item{title}{
(\code{character}) title of the data set. 
}
\item{documentation}{
(\code{character}) user-supplied documentation. 
}
\item{attributes}{
(\code{ANY}) attributes slot for arbitrary use. 
}
}
}
\details{
The \code{series} class holds x positions and variable data.  
It is valid only when the lengths of the positions and data match, 
and when the data slot is a rectangular object. 

\code{seriesVirtual} is a virtual class 
corresponding to \code{series}. All of the methods 
for \code{series} objects are defined on the corresponding virtual 
\code{seriesVirtual} class  so they can be inherited easily 
by extending classes.  

\code{series} has two built-in extending classes:  
\code{timeSeries} and \code{signalSeries}.   \code{series} is not meant to be used directly. Instead, most users should use the 
\code{signalSeries} and \code{timeSeries} classes. Extending classes 
should include both \code{series} and \code{seriesVirtual} in their representations. 
}
\section{Series functions}{
\itemize{
\item The \code{series} class has a validity function, \code{seriesValid}.  
\item The access functions \code{positions} and \code{seriesData}  
can access the positions and data in the object, and they can be used 
on the left side of assignments. 
\item There are also methods defined for \code{series} objects for the  
following functions: 
\itemize{
\item \code{nrow} 
\item \code{ncol} 
\item \code{start}
\item \code{end}
\item subscripting
\item the standard rectangular data functions (see \code{is.rectangular}) 
\item  basic arithmetic. 
}
}
}
\seealso{
\code{\linkS4class{timeSeries}}  class, \code{\linkS4class{signalSeries}}  class, \code{\link{is.rectangular}}.   
}
\keyword{ts}
\keyword{classes}
% docclass is function
% Converted by mySd2Rd version 47442.
