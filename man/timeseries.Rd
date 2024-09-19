\name{timeSeries}
\alias{timeSeries}
% this suppresses a warning message
\docType{data}
\title{
Create a \code{timeSeries} Object
}
\description{
Creates an object of class \code{timeSeries}.
}
\usage{
timeSeries(data, positions., units., from = timeCalendar(d = 1,
    m = 1, y = 1960), by = "days", k.by = 1, align.by = FALSE,
    week.align = NULL)
}
\arguments{
  \item{data}{
    (\code{ANY}) the variable data. Can be any data object for which
    \code{is.rectangular} is \code{TRUE},
    such as a \code{data.frame}, \code{matrix}, or atomic vector.
  }
  \item{positions.}{
    (\code{positions}) the x values for the variables.
    Must be of type \code{positionsCalendar}.
    If not given, then the \code{positions} are computed using the
    \code{timeSeq} function with the
    \code{from}, \code{by}, \code{k.by}, \code{align} and \code{week.align} arguments.
  }
  \item{units.}{
    (\code{character}) the units for the data.
  }
  \item{from}{
    the starting value of the sequence. A \code{timeDate} object (or number or
    character string representing one).
  }
  \item{by}{
    the spacing between successive values in the sequence. Can be
    a \code{timeSpan}, \code{timeRelative}, or \code{numeric} value,
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
    giving the time units of intervals between values in the sequence.
  }
  \item{k.by}{
    a non-zero integer giving the width of the interval between consecutive
    values in the sequence in terms of the units given in \code{by}.  Ignored if
    \code{by} is not a character string.
  }
  \item{align.by}{
    a logical value. If \code{TRUE}, adjusts the sequence so that each element is
    on a whole number of the \code{by * k.by} units. For example, if the units 
    are 2 months, the sequence is only on the first of January, March, and so on.
    Ignored if \code{by} is not a character string.
  }
  \item{week.align}{
    if \code{by} is \code{"weeks"}, you can supply a character
    string (or a number, 0 to 6 with 0 being Sunday)
    to specify a weekday to use. (The character string must
    be sufficient to make a unique case-insensitive match to the
    strings in \code{timeDateOptions("time.day.name")}.)
    \itemize{
    \item If \code{align.by} is \code{FALSE}, the sequence is adjusted 
    so that all its elements fall on the given weekday. 
    \item If \code{align.by} is \code{TRUE}, the sequence is adjusted 
    to start at midnight. 
    }
    In either case, the \code{extend} argument is used to decide which 
    direction to adjust the day. This argument is ignored if \code{by} 
    is not a character string, or if it is not \code{"weeks"}.
  }
}
\value{
an object of class \code{"timeSeries"}.
See the
\code{\linkS4class{timeSeries}} class help file
for the names and structure of the slots in the object.
}
\seealso{
\code{\linkS4class{timeSeries}} class.
}
\examples{
timeSeries(pos=timeCalendar(d=1:10), data=1:10)
timeSeries(data=data.frame(x=1:10, y=11:20), from="7/4/2000", by="bizdays")
}
\keyword{ts}
