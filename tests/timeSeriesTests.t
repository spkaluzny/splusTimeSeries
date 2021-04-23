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
     row.names(y@data) <- row.names(x@data)
     row.names(x@data) <- row.names(y@data)
     all.equal(x,y)
  }
  TRUE
}

{
  # test time series creation

  a <- new( "timeSeries" )
  b <- timeSeries()
  validObject(a)
  all.equal(a,b)
}

{
  # test timeSeries function
  a <- timeSeries( pos=timeCalendar( d=1:10 ), data = 1:10)
  b <- timeSeries( data = 1:10, from=timeCalendar(d=1))
  validObject( a )
  validObject( b )
  ( all( positions(a) == timeCalendar( d = 1:10 )) &&
    all( seriesData(a) == 1:10 ) &&
    all( positions(a) == positions(b)) &&
    all( seriesData(b) == 1:10 ))
}

{
  # test auxiliary functions
  a <- timeSeries( pos=timeCalendar( d=1:10 ), 
	data = data.frame(a=1:10,b=1:10))
  b <- timeSeries( pos=timeCalendar( d=1:10 ), 
	data = 1:10 )
  aa <- timeSeries( pos= timeCalendar( d=1:10 ), 
	data = matrix(1:20, ncol=2))

  ( ( nrow( a ) == 10 ) &&
    ( ncol( a ) == 2 ) && 
    ( start( a ) == timeCalendar( d = 1 )) &&
    ( end( a ) == timeCalendar( d = 10 )) &&
    ( nrow( aa ) == 10 ) &&
    ( ncol( aa ) == 2 ) && 
    ( nrow( b ) == 10 ) &&
    ( ncol( b ) == 1 ))
}

{
  # test subscripting
  a <- timeSeries( pos= timeCalendar( d = 1:10 ), 
                  data = data.frame( a = 1:10, b = 11:20 ))
  b <- a
  b[,2] <- 41:50
  b[1,] <- 3

    compare.series( a[,2], timeSeries( pos= timeCalendar( d = 1:10 ), 
				   data.frame( b = 11:20 )))
}
{
  # test subscripting #2
    compare.series( a[3,], timeSeries( pos= timeCalendar( d = 3 ), 
				  a@data[3,,drop=FALSE] ))
}
{
  # test subscripting #3
    compare.series( b[,2], timeSeries( pos= timeCalendar( d = 1:10 ), 
				   data.frame( b = c( 3, 42:50 ))))
}
{
  # test subscripting #4
    compare.series( b[1,], timeSeries( pos= timeCalendar( d = 1 ), 
				   data.frame( a = 3, b = 3 )))
}

{
  # test subscripting with vector
  a <- timeSeries( pos= timeCalendar( d = 1:10 ), 
		    data = 1:10 )
  b <- a
  b[,1] <- 41:50
  b[1,] <- 3

  compare.series( a[,1], a )
}
{
  # test subscripting with vector #2

    compare.series( a[3,], timeSeries( pos= timeCalendar( d = 3 ), 3 ))
}
{
  # test subscripting with vector #3

    compare.series( b[,1], timeSeries( pos= timeCalendar( d = 1:10 ), 
	data= c(3, 42:50 )))
}
{
  # test subscripting with vector #4

    compare.series( b[1,], timeSeries( pos= timeCalendar( d = 1 ), 3 ))
}

{
  # test subscripting with matrix
  a <- timeSeries( pos= timeCalendar( d = 1:10 ), 
		    data = matrix(1:20, ncol=2))
  b <- a
  b[,2] <- 41:50
  b[1,] <- 3

   compare.series( a[,2], timeSeries( pos= timeCalendar( d = 1:10 ), 
	data=matrix( 11:20 )))
}
{
  # test subscripting with matrix #2
  compare.series( a[3,], timeSeries( pos= timeCalendar( d = 3 ), 
                                    a@data[3,,drop=FALSE]))
}
{
  # test subscripting with matrix #3

    compare.series( b[,2], timeSeries( pos= timeCalendar( d = 1:10 ), 
	data=matrix( c( 3, 42:50 ))))
}
{
  # test subscripting with matrix #4

    compare.series( b[1,], timeSeries( pos= timeCalendar( d = 1 ), 
	matrix(c( 3, 3), nrow=1 )))
}

{
  # test math functions -- sin

  a <- data.frame( a = 1:10, b = 11:20 )
  apos <- timeCalendar( d = 1:10 )
  b <- timeSeries( pos= apos, a )
    compare.series( sin( b ), timeSeries( pos= apos, sin( a )))
}
{
  # test math functions -- logb

    compare.series( logb( b, 5 ), timeSeries( pos= apos, logb( a, 5 )))
}
{
  # test math functions -- round

    compare.series( round( b / 1000, 2 ), 
	       timeSeries( pos= apos, round( a / 1000, 2 )))
}
{
  # test math functions -- range

    all( range( a ) == range( b ))
}
{
  # test math functions -- arithmetic 
    compare.series( b / 4 - 4 * b, timeSeries( pos= apos, a / 4 - 4 * a ))
}
{
  # test math functions -- comparison

    compare.series( b >= 7, timeSeries( pos= apos, a >= 7 ))
}

{
  # test math functions with matrix
  a <- matrix(1:20, nrow=10)
  apos <- timeCalendar( d = 1:10 )
  b <- timeSeries( pos= apos, a )
  compare.series( sin( b ), timeSeries( pos= apos, sin( a )))
}
{
  # test math functions with matrix -- logb

    compare.series( logb( b, 5 ), timeSeries( pos= apos, logb( a, 5 )))
}
{
  # test math functions with matrix -- round

    compare.series( round( b / 1000, 2 ), 
	       timeSeries( pos= apos, round( a / 1000, 2 )))
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
  # test math functions with matrix -- range 
    all( range( a ) == range( b ))
}
{
  # test math functions with matrix -- arithmetic

    compare.series( b / 4 - 4 * b, timeSeries( pos= apos, a / 4 - 4 * a ))
}
{
  # test math functions with matrix -- compare
    compare.series( b >= 7, timeSeries( pos= apos, a >= 7 ))
}

{
  # test math functions with vector
  a <- 1:10 
  apos <- timeCalendar( d = 1:10 )
  b <- timeSeries( pos= apos, a )
  compare.series( sin( b ), timeSeries( pos= apos, sin( a )))
}
{
  # test math functions with vector -- log
    compare.series( logb( b, 5 ), timeSeries( pos= apos, logb( a, 5 )))
}
{
  # test math functions with vector -- round
    compare.series( round( b / 1000, 2 ), 
	       timeSeries( pos= apos, round( a / 1000, 2 )))
}
{
  # test math functions with vector -- range

    all( range( a ) == range( b ))
}
{
  # test math functions with vector -- arithmetic
    compare.series( b / 4 - 4 * b, timeSeries( pos= apos, a / 4 - 4 * a ))
}
{
  # test math functions with vector -- compare
    compare.series( b >= 7, timeSeries( pos= apos, a >= 7 ))
}

{
  # test diff
  a <- timeSeries( data=1:10, pos= as(1:10, "timeDate"))
  b <- c( a, timeSeries( data= 11:20, pos=as(11:20, "timeDate")))

    all( seriesData(diff(a)) == 1 )
}
{
 # test concat
    all( seriesData(b) == 1:20 )
}
{
# test concat # 2
    all( positions(b) == as(1:20, "timeDate") )
}
{
   # test shift    
   all( positions(shift(a)) == as( 2:11, "timeDate") )
}

all.equal(
    structure(list(TS = 1:9), row.names = c("03/01/1996", "03/01/1997",
        "03/01/1998", "03/01/1999", "03/01/2000", "03/01/2001", "03/01/2002",
        "03/01/2003", "03/01/2004"), class = "data.frame"),
    data.frame(TS=timeSeries(1:9, from=timeCalendar(d=1, m=3, y=1996), by="years")))
