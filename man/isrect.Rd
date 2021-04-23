\name{as.rectangular}
\alias{as.rectangular}
\alias{as.char.rect}
\alias{is.rectangular}
\alias{subscript2d}
\alias{subscript2d,ANY-method}
\alias{subscript2d,data.frame-method}
\alias{subscript2d,matrix-method}
\alias{subscript2d<-}
\alias{subscript2d<-,ANY-method}
\alias{subscript2d<-,data.frame-method}
\alias{subscript2d<-,matrix-method}
\alias{numRows}
\alias{numRows,ANY-method}
\alias{numRows<-}
\alias{numRows<-,ANY-method}
\alias{numRows<-,data.frame-method}
\alias{nrow<-}
\alias{nrow<-,ANY-method}
\alias{nrow<-,data.frame-method}
\alias{numCols}
\alias{numCols,ANY-method}
\alias{numCols<-}
\alias{numCols<-,ANY-method}
\alias{numCols<-,data.frame-method}
\alias{ncol<-}
\alias{ncol<-,ANY-method}
\alias{ncol<-,data.frame-method}
\alias{rowIds}
\alias{rowIds,ANY-method}
\alias{rowIds<-}
\alias{rowIds<-,ANY-method}
\alias{colIds}
\alias{colIds,ANY-method}
\alias{colIds<-}
\alias{colIds<-,ANY-method}
\title{
Uniform Rectangular Data Functions 
}
\description{
Functions that allow you to access all rectangular data objects in 
the same way. Rectangular data objects include matrices, data frames
and vectors.
}
\usage{
as.rectangular(x) 
as.char.rect(x) 
is.rectangular(x) 
subscript2d(x,i,j) 
subscript2d(x,i,j) <- value
numRows(x) 
numRows(x) <- value
numCols(x) 
numCols(x) <- value
rowIds(x) 
rowIds(x) <- value
colIds(x) 
colIds(x) <- value
}
\arguments{
\item{x}{
the object to be converted to rectangular data (\code{as.rectangular}), or a 
rectangular data object. 
}
\item{i}{
the first (row) subscript. 
}
\item{j}{
the second (column) subscript. 
}
\item{value}{
the object to be assign to \code{x}
}
}
\value{
\item{\code{as.rectangular}}{returns \code{x} if it is already rectangular, or 
\code{as.data.frame(x)} if it is not.}
\item{\code{as.char.rect}}{takes a rectangular object and returns a rectangular object
(vector or matrix) consisting of character strings, suitable for printing 
(but not formatted to fixed width).}
\item{\code{is.rectangular}}{returns \code{TRUE} if \code{x} is rectangular, and \code{FALSE} if it is not.}
\item{\code{subscript2d(x,i,j)}}{is like \code{x[i,j,drop=F]}, except that it allows 
\code{x[,1]} (for example) for atomic vectors.
Usually, it returns an object of the same class as \code{x}
(this is not appropriate for some objects, such as \code{"bs"} objects).
It does not support a \code{drop} argument.}
\item{\code{numRows} and \code{numCols}}{return integers, like \code{nrow} and \code{ncol}, 
except that they also work on atomic vectors (\code{numRows} returns 
the length of the vector, and \code{numCols} returns 1).}
\item{\code{rowIds} and \code{colIds}}{return the IDs of the rows and columns. 
These are often character vectors, but need not be,
depending on the class of \code{x}.
They are like the components of \code{dimnames},
except that for named vectors, \code{rowIds} returns or sets the names
and \code{colIds} returns \code{NULL}.} 
\item{\code{colnames} and \code{rownames}}{return the same values as 
\code{colIds} and \code{rowIds}, respectively.}
}
\details{
\code{subscript2d}, \code{numRows}, \code{numCols}, \code{rowIds},
\code{colIds} can also be used on the left side of assignments. The \code{value} can be a character vector, or anything that
can be coerced to a character vector.

\itemize{
\item \code{subscript2d} is for subscripting. When \code{subscript2d} is used in an assignment, it does not allow subscript replacement 
outside the bounds of \code{x}. Instead, set \code{numRows} or 
\code{numCols} first. 
\item When \code{numRows} or \code{numCols} is used in
an assignment, the row and column IDs are maintained 
to have the correct length. Usually, this is done by setting \code{numRows} on the ID vector,
but for some objects (for example, data frames)
this might not be appropriate, and they have their own methods.
\item Functions \code{colnames<-} and \code{rownames<-} simply call 
\code{colIds<-} and \code{rowIds<-}, respectively.
\item \code{as.rectangular} converts any object to a rectangular 
data object (usually a data frame), if possible.
\item \code{is.rectangular} tests whether an object is rectangular. 
\item \code{numRows} and \code{numCols} count the number of rows and columns. 
\item \code{rowIds} and \code{colIds} (and \code{rownames} and 
\code{colnames}) return the row and column names or other
identifiers attached to rows and columns.
\item \code{colnames} and \code{rownames} return the same values as 
\code{colIds} and \code{rowIds}, respectively, if \code{do.NULL=T}.
\item Instead of using \code{names} to replace row names from a matrix,
use \code{rowIds} or \code{dimnames}. 
\item The functions \code{colnames}, \code{rownames},
\code{colnames<-}, \code{rownames<-} emulate \bold{R} 
functions of the same names.
}
}
\seealso{
\code{\link{as.data.frame}}, \code{\link{matrix}}, \code{\link{Subscript}}, \code{\link{nrow}}, \code{\link{dimnames}}.
}
\examples{
x <- 1:10 
y <- list(a=1:10, b=11:20) 
is.rectangular(x) 
y <- as.rectangular(y) 
subscript2d(x,3,1) 
subscript2d(y,4,1) <- 55 
numRows(x) 
numCols(y) <- 3 
rowIds(x) <- letters[1:10] 
colIds(y) 
z <- cbind(1,1:4)
colnames(z)
colnames(z) <- colnames(z)
rownames(z) <- rownames(z)
}
\keyword{manip}

