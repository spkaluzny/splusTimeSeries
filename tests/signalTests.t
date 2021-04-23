{
  library(splusTimeSeries)
  TRUE
}

{
  # define compare function -- compare two series but ignore row names
  compare.series <- function(x,y)
  {
     if( ncol(x) != ncol(y))
       return( "different number of rows" )
     rowIds(y@data) <- rowIds(x@data)
     rowIds(x@data) <- rowIds(y@data)
     all.equal(x,y)
  }
  TRUE
}

{
  # test signal creation

  a <- new( "signalSeries" )
  b <- signalSeries()
  validObject(a)
  all.equal(a,b)
}

{
  # test signalSeries function
  a <- signalSeries( data = 1:10, from = 1, by = 1 )
  validObject( a )
    compare.series( a, 
	signalSeries( pos=numericSequence( 1, by=1, length = 10 ), 
	data = 1:10 ))
}
{
  # test signalSeries function continued

    ( all( positions(a) == 1:10 ) &&
    all( seriesData(a) == 1:10 ))
}

{
  # test auxiliary functions
  a <- signalSeries( data = data.frame( a = 1:10, b = 1:10 ), from = 5, 
	       by = 1 )
  b <- signalSeries( data = 1:10, from = 5, by = 1 )
  aa <- signalSeries( data = matrix(1:20, ncol=2), from = 5, by = 1 )

  ## start and end fail
  ( ( nrow( a ) == 10 ) &&
    ( ncol( a ) == 2 ) && 
    ( start( a ) == 5 ) &&
    ( end( a ) == 14 ) &&
    ( nrow( aa ) == 10 ) &&
    ( ncol( aa ) == 2 ) && 
    ( start( aa ) == 5 ) &&
    ( end( aa ) == 14 ) &&
    ( nrow( b ) == 10 ) &&
    ( ncol( b ) == 1 ) && 
    ( start( a ) == 5 ) &&
    ( end( a ) == 14 ))

}

{
  # test subscripting
  a <- signalSeries( data = data.frame( a = 1:10, b = 11:20 ), from = 5, 
	       by = 1 )
  b <- a
  b[,2] <- 41:50
  b[1,] <- 3

  compare.series( a[,2], signalSeries( from=5, by =1,
	data=data.frame( b = 11:20 )))
}
{
  # test subscripting function # 2

    compare.series( a[3,], signalSeries( pos=7., a@data[3,,drop=FALSE]))
}
{
  # test subscripting function # 3
    compare.series( b[,2], signalSeries( from=5, by = 1, 
	data=data.frame( b = c( 3, 42:50 ))))
}
{
  # test subscripting function # 4

    compare.series( b[1,], signalSeries( pos=5., data.frame( a = 3, b = 3 )))
}
{
  # test subscripting with matrix
  a <- signalSeries( data = matrix(1:20, ncol=2), from = 5, by = 1 )
  b <- a
  b[,2] <- 41:50
  b[1,] <- 3

   compare.series( a[,2], signalSeries( from=5, by =1,
	data=matrix( 11:20 )))
}
{
  # test subscripting function # 2 with matrix
    compare.series( a[3,], signalSeries( pos=7., a@data[3,,drop=FALSE]))
}
{
  # test subscripting function # 3 with matrix 
    compare.series( b[,2], signalSeries( from=5, by = 1, 
	data=matrix( c( 3, 42:50 ))))
}
{
  # test subscripting function # 4 with matrix
    compare.series( b[1,], signalSeries( pos=5., matrix(c( 3, 3), nrow=1 )))
}

{
  # test subscripting with vector
  a <- signalSeries( data = 1:10, from = 5, by = 1 )

  b <- a
  b[,1] <- 41:50
  b[1,] <- 3

   compare.series( a[,1], a )
}
{
  # test subscripting function # 2 with vector
    compare.series( a[3,], signalSeries( pos=7., 3 ))
}
{
  # test subscripting function # 3 with vector
    compare.series( b[,1], signalSeries( from=5, by = 1, 
	data= c(3, 42:50 )))
}
{
  # test subscripting function # 4 with vector
    compare.series( b[1,], signalSeries( pos=5., 3 ))
}

{
  # test math functions
  a <- data.frame( a = 1:10, b = 11:20 )
  b <- signalSeries( pos=1:10, a )
  compare.series( sin( b ), signalSeries( pos=1:10, sin( a )))
}
{
  # test logb
    compare.series( logb( b, 5 ), signalSeries( pos=1:10, logb( a, 5 )))
}
{ 
  # test round
   compare.series( round( b / 1000, 2 ), signalSeries( pos=1:10, round( a / 1000, 2 ))) 
}
{
   # test range
    all( range( a ) == range( b ))
}
{
  # test arithmetic
    compare.series( b / 4 - 4 * b, signalSeries( pos=1:10, a / 4 - 4 * a ))
}
{
  # test comparison
    compare.series( b >= 7, signalSeries( pos=1:10, a >= 7 ))
}

{
  # test math functions with matrix
  a <- matrix( 1:20, nrow=10 )
  b <- signalSeries( pos=1:10, a )
  compare.series( sin( b ), signalSeries( pos=1:10, sin( a )))
}
{
  # test logb with matrix 
    compare.series( logb( b, 5 ), signalSeries( pos=1:10, logb( a, 5 )))
}
{
  # test round with matrix
  compare.series( round( b / 1000, 2 ), signalSeries( pos=1:10, round( a / 1000, 2 )))
}
{
  # test math functions with matrix -- mean, median, quantile

  ( all.equal( mean( b ), mean( a )) &&
    all.equal( median( b ), median( a )) &&
    all.equal( quantile( b ), quantile( a )) &&
    all.equal( var( b ), var( a )) &&
    all.equal( cor( b ), cor( a )))

}
{
  # test range with matrix
    all( range( a ) == range( b ))
}
{
  # test arith with matrix
    compare.series( b / 4 - 4 * b, signalSeries( pos=1:10, a / 4 - 4 * a ))
}
{
  # test compare with matrix
    compare.series( b >= 7, signalSeries( pos=1:10, a >= 7 ))
}

{
  # test math functions with vector
  a <- 1:10
  b <- signalSeries( pos=1:10, a )
  compare.series( sin( b ), signalSeries( pos=1:10, sin( a )))
}
{
  # test logb with vector
    compare.series( logb( b, 5 ), signalSeries( pos=1:10, logb( a, 5 )))
}
{
  # test round with vector
  compare.series( round( b / 1000, 2 ), signalSeries( pos=1:10, round( a / 1000, 2 )))
}
{
  # test range with vector 

  all( range( a ) == range( b )) 
}
{
  # test arith with vector
    compare.series( b / 4 - 4 * b, signalSeries( pos=1:10, a / 4 - 4 * a ))
}
{
  # test compare with vector
    compare.series( b >= 7, signalSeries( pos=1:10, a >= 7 ))
}

{
  # test diff/concat/shift
  a <- signalSeries( data=1:10, pos=1:10 )
  b <- c( a, signalSeries( data= 11:20, pos=11:20))

  ( all( seriesData(diff(a)) == 1 ) &&
    all( seriesData(b) == 1:20 ) &&
    all( positions(b) == 1:20 ) &&
    all( positions(shift(a)) == 2:11 ))
}

all.equal(
    structure(list(SS = 1:5), row.names = c("1001", "1002", "1003", "1004", "1005"), class = "data.frame"),
    data.frame(SS=signalSeries(1:5, 1001:1005, units="smoots", units.position="furlongs")))
