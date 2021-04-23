\name{signalSeries}
\alias{signalSeries}
\title{
Create a \code{signalSeries} object
}
\description{
Creates an object of class \code{signalSeries}
}
\usage{
signalSeries(data, positions., units, units.position, from = 1, by = 1)
}
\arguments{
  \item{data}{
    (\code{ANY}) the variable data, which can be any data object for which
    \code{is.rectangular} is \code{TRUE},
    such as a \code{data.frame}, \code{matrix}, or atomic vector.
  }
  \item{positions.}{
    (\code{positions}) the x values for the variables, which must be
    of type \code{positionsNumeric}.
    If not given, then the \code{positions} are computed using the
    \code{numSeq} function with the \code{from} and \code{by}.
  }
  \item{units}{
    (\code{character}) the units for the data.
  }
  \item{units.position}{
    (\code{character}) the units for the \code{positions} slot.
  }
  \item{from}{
    the start of the sequence.
  }
  \item{by}{
    the increment for the sequence.
  }
}
\seealso{
  \code{\linkS4class{signalSeries}} class,
  \code{\linkS4class{numericSequence}} class.
}
\examples{
signalSeries( pos=1:10 , data = 1:10)
signalSeries(data=data.frame(x=1:10, y=11:20), from=2, by=2)
}
\keyword{ts}
