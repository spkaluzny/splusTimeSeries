{
  library(splusTimeSeries)
  TRUE
}

{
	save.options <- timeDateOptions(time.zone="GMT")
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
     if( is( x@positions, "timeDate" ))
	x@positions@format <- y@positions@format
     all.equal(x,y)
  }
  TRUE
}

{
  # test signalSeries align with how = NA

  adat1 <- data.frame( a = 11:20, b = 5 * (1:10 ))
  adat2 <- matrix( 1:20, nrow=10 )
  adat3 <- 1:10
  apos <- 1:10
  aunits <- c( "mm", "cm" )
  alpos <- c( .2, 3, 7.8, 12 )

  a <- signalSeries( pos=apos, data=adat1, units = aunits )
  compare.series( align( a, alpos, how = "NA" ),
       signalSeries( pos=alpos, data.frame( a = c( NA, 13, NA, NA ),
                       b = c( NA, 15, NA, NA )),units=aunits))
}

{
  # test signalSeries alignment with how = drop
  a <- signalSeries( pos=apos, data=adat2, units = aunits )
  x <- align( a, alpos, how = "drop" )
  y <- signalSeries( pos=alpos[2], data=adat2[3,,drop=FALSE],units=aunits)
  compare.series( x,y)
}

{
  # test signalSeries alignment with how = nearest
  a <- signalSeries( pos=apos, data=adat3, units = aunits )
  compare.series( align( a, alpos, how = "nearest" ),
	     signalSeries( pos=alpos, c( 1, 3, 8, 10 ),
				  units=aunits))
}

{
  # test signalSeries alignment when input sig is short
  a <- signalSeries( pos=apos, data=adat1, units = aunits )
  b <- a[1:3,]
  compare.series( align( b, alpos, how = "NA" ),
	     signalSeries( pos=alpos, data.frame( a = c( NA, 13, NA, NA ),
				        b = c( NA, 15, NA, NA )),units=aunits))
}

{
  # test signalSeries alignment with how = before
  a <- signalSeries( pos=apos, data=adat2, units = aunits )
  compare.series( align( a, alpos, how = "before", error.how = "nearest" ),
	     signalSeries( pos=alpos, matrix( c( 1, 3, 7, 10, 11, 13, 17, 20), 
				nrow=4 ), units=aunits))
}

{
  # test signalSeries alignment with how = after
  a <- signalSeries( pos=apos, data=adat3, units = aunits )
  compare.series( align( a, alpos, how = "after", error.how = "drop" ),
	     signalSeries( pos=alpos[-4], c( 1, 3, 8 ), units=aunits))
}

{
  # test signalSeries alignment with how = interp, data frame
  a <- signalSeries( pos=apos, data=adat1, units = aunits )
  x <- align( a, alpos, how = "interp", error.how = "nearest" )
  y <- signalSeries( pos=alpos,data.frame(a = c(11, 13, 17.8, 20), b = c(5.,
                15, 39, 50)),units=aunits)
  compare.series( x,y)
}

{
  # test signalSeries alignment with how = interp, matrix
  a <- signalSeries( pos=apos, data=adat2, units = aunits )
  x <- align( a, alpos, how = "interp", error.how = "nearest" )
  y <- signalSeries( pos=alpos,matrix( c(1, 3, 7.8, 10, 11, 13, 17.8, 20), 
	ncol=2 ),units=aunits)
  compare.series( x,y)
}

{
  # test signalSeries alignment with how = interp, vector
  a <- signalSeries( pos=apos, data=adat3, units = aunits )
  x <- align( a, alpos, how = "interp", error.how = "nearest" )
  y <- signalSeries( pos=alpos, c(1, 3, 7.8, 10),units=aunits)
  compare.series( x,y)
}

{
  # test signalSeries alignment with non-zero match tolerance
  a <- signalSeries( pos=apos, data=adat1, units = aunits )
  compare.series( align( a, alpos, how = "NA", matchtol = 1.3 ),
	     signalSeries( pos=alpos, data.frame( a = c( 11, 13, 18, NA ),
				        b = c( 5, 15, 40, NA )),units=aunits))
  
}

{
  # test signalSeries seriesMerge
  b <- a
  dimnames( b@data )[[2]] <- c( "a", "bbb" )
  positions( b ) <- positions( a ) + 5
  b@units <- c( "cm", "ml" )

  compare.series( seriesMerge( a, b ), 
	     signalSeries( pos=6:10, data.frame( a.1 = 16:20, b = 5 * (6:10 ),
				       a.2 = 11:15, bbb = 5 * (1:5 )),
		     units = c( aunits, "cm", "ml" )))
}
{
  # seriesMerge test 2 on signalSeries
    compare.series( seriesMerge( a, b, pos = "union" ), 
	     signalSeries( pos=1:15, data.frame( a.1 = c(11:20,rep(NA,5)), 
				       b = c(5 * (1:10 ), rep(NA,5)),
				       a.2 = c(rep(NA,5),11:20), 
				       bbb = c(rep(NA,5), 5 * (1:10))),
		     units = c( aunits, "cm", "ml" )))
}

{
  # test signalSeries seriesMerge with matrix
  a <- signalSeries( pos=apos, data=adat2, units = aunits )
  b <- a
  dimnames( b@data ) <- list( character(0), c( "a", "bbb" ))
  dimnames( a@data ) <- list( character(0), c( "a", "b" ))
  positions( b ) <- positions( a ) + 5
  b@units <- c( "cm", "ml" )

  compare.series( seriesMerge( a, b ), 
	     signalSeries( pos=6:10, matrix( c( 6:10, 16:20, 1:5, 11:15 ),
				nrow=5, dimnames=list(character(0),
				c( "a.1", "b", "a.2", "bbb" ))),
		     units = c( aunits, "cm", "ml" )))
}
{
  # signalSeries seriesMerge with matrix test 2
    compare.series( seriesMerge( a, b, pos = "union" ), 
	     signalSeries( pos=1:15, matrix( c(1:10,rep(NA,5),
				       11:20, rep(NA,5),
				       rep(NA,5), 1:10, 
				       rep(NA,5), 11:20 ),
				nrow=15, dimnames=list(character(0),
				c( "a.1", "b", "a.2", "bbb" ))),
		     units = c( aunits, "cm", "ml" )))
}

{
  # test timeSeries align with how = NA
  apos <- as( apos, "timeDate" )
  alpos <- as( alpos, "timeDate" )
  a <- timeSeries( pos=apos, data=adat2, units = aunits )
  compare.series( align( a, alpos, how = "NA" ),
	     timeSeries( pos=alpos, matrix( c( NA, 3, NA, NA, NA, 13, NA, NA ),
			ncol=2),units=aunits))
}

{
  # test timeSeries alignment with how = drop

  a <- timeSeries( pos=apos, data=adat3, units = aunits )
  x <- align( a, alpos, how = "drop" )
  y <- timeSeries( pos=alpos[2], data=adat3[3],units=aunits)
  compare.series( x,y)
}

{
  # test timeSeries alignment with how = nearest
  a <- timeSeries( pos=apos, data=adat1, units = aunits )
  compare.series( align( a, alpos, how = "nearest" ),
	     timeSeries( pos=alpos, data.frame( a = c( 11, 13, 18, 20 ),
				        b = c( 5, 15, 40, 50 )),units=aunits))
}

{
  # test timeSeries alignment when input series is short
  b <- a[1:3,]
  compare.series( align( b, alpos, how = "NA" ),
	     timeSeries( pos=alpos, data.frame( a = c( NA, 13, NA, NA ),
				        b = c( NA, 15, NA, NA )),units=aunits))
}

{
  # test timeSeries alignment with how = before
  a <- timeSeries( pos=apos, data=adat3, units = aunits )
  compare.series( align( a, alpos, how = "before", error.how = "nearest" ),
	     timeSeries( pos=alpos, c( 1, 3, 7, 10 ), units=aunits))
}

{
  # test timeSeries alignment with how = after
  a <- timeSeries( pos=apos, data=adat1, units = aunits )
  compare.series( align( a, alpos, how = "after", error.how = "drop" ),
	     timeSeries( pos=alpos[-4], data.frame( a = c( 11, 13, 18 ),
				        b = c( 5, 15, 40 )),units=aunits))
}

{
  # test timeSeries alignment with how = interp
  a <- timeSeries( pos=apos, data=adat1, units = aunits )
  x <- align( a, alpos, how = "interp", error.how = "nearest" )
  y <- timeSeries( pos=alpos, data.frame( a = c( 11, 13, 17.8, 20 ),
				        b = c( 5., 15, 39, 50 )),units=aunits)
  compare.series( x,y)
}

{
  # test timeSeries alignment with how = interp, matrix
  a <- timeSeries( pos=apos, data=adat2, units = aunits )
  x <- align( a, alpos, how = "interp", error.how = "nearest" )
  y <- timeSeries( pos=alpos, matrix( c( 1, 3, 7.8, 10, 11, 13, 17.8, 20 ),
				ncol = 2 ), units=aunits)
  compare.series( x,y)
}

{
  # test timeSeries alignment with how = interp
  a <- timeSeries( pos=apos, data=adat3, units = aunits )
  x <- align( a, alpos, how = "interp", error.how = "nearest" )
  y <- timeSeries( pos=alpos, c( 1, 3, 7.8, 10 ), units=aunits)
  compare.series( x,y)
}


{
  # test timeSeries alignment with non-zero match tolerance
  a <- timeSeries( pos=apos, data=adat1, units = aunits )
  compare.series( align( a, alpos, how = "NA", matchtol = 1.3 ),
	     timeSeries( pos=alpos, data.frame( a = c( 11, 13, 18, NA ),
				        b = c( 5, 15, 40, NA )),units=aunits))
  
}

{
  # test timeSeries alignment with localzone
  alpos@time.zone <- "JST"
  positions(a)@time.zone <- "PST"

  compare.series( align( a, alpos, how = "NA", matchtol = 1, localzone = TRUE ),
	     timeSeries( pos=alpos, 
			  data.frame( a = c( 11, 14, 19, NA ),
				      b = c( 5, 20, 45, NA )),
			  units = aunits ))

}

{
  # test timeSeries seriesMerge
  positions(a) <- apos
  b <- a
  dimnames( b@data )[[2]] <- c( "a", "bbb" )
  positions( b ) <- positions( a ) + 5
  b@units <- c( "cm", "ml" )

  compare.series( seriesMerge( a, b ), 
	     timeSeries( pos=as(6:10,"timeDate"), 
			  data.frame( a.1 = 16:20, b = 5 * (6:10 ),
				       a.2 = 11:15, bbb = 5 * (1:5 )),
		     units = c( aunits, "cm", "ml" )))
}
{
  # timeSeries seriesMerge test 2
    compare.series( seriesMerge( a, b, pos = "union" ), 
	     timeSeries( pos=as( 1:15, "timeDate"), 
			  data.frame( a.1 = c(11:20,rep(NA,5)), 
				       b = c(5 * (1:10 ), rep(NA,5)),
				       a.2 = c(rep(NA,5),11:20), 
				       bbb = c(rep(NA,5), 5 * (1:10))),
		     units = c( aunits, "cm", "ml" )))
}

{
  # test timeSeries seriesMerge -- matrix
  a <- timeSeries( pos=apos, data=adat2, units = aunits )
  b <- a
  dimnames( b@data ) <- list( character(0), c( "a", "bbb" ))
  dimnames( a@data ) <- list( character(0), c( "a", "b" ))
  positions( b ) <- positions( a ) + 5
  b@units <- c( "cm", "ml" )

  compare.series( seriesMerge( a, b ), 
	     timeSeries( pos=as(6:10,"timeDate"), 
			matrix( c( 6:10, 16:20, 1:5, 11:15 ),
				nrow=5, dimnames=list(character(0),
				c( "a.1", "b", "a.2", "bbb" ))),
		     units = c( aunits, "cm", "ml" )))
}
{
  # timeSeries seriesMerge with matrix test 2

    compare.series( seriesMerge( a, b, pos = "union" ), 
	     timeSeries( pos=as( 1:15, "timeDate"), 
				matrix( c(1:10,rep(NA,5),
				       11:20, rep(NA,5),
				       rep(NA,5), 1:10, 
				       rep(NA,5), 11:20 ),
				nrow=15, dimnames=list(character(0),
				c( "a.1", "b", "a.2", "bbb" ))),
		     units = c( aunits, "cm", "ml" )))
}

{
  # test timeSeries seriesMerge -- vector
  a <- timeSeries( pos=apos, data=adat3, units = aunits )
  b <- a
  positions( b ) <- positions( a ) + 5
  b@units <- c( "cm", "ml" )
  
  compare.series( seriesMerge( a, b ), 
	     timeSeries( pos=as(6:10,"timeDate"), 
			matrix( c( 6:10, 1:5 ), nrow=5,
				dimnames=list(character(0), c( "1.1", "1.2"))),
		     units = c( aunits, "cm", "ml" )))
}
{
  # test timeSeries seriesMerge -- vector test 2
  pos =  unionPositions(c(a@positions, b@positions))
  x = align(a, pos, how="NA", error.how="NA")
  x = seriesMerge( a, b, pos = "union" )
    compare.series( seriesMerge( a, b, pos = "union" ), 
	     timeSeries( pos=as( 1:15, "timeDate"), 
				matrix( c(1:10,rep(NA,5),
				       rep(NA,5), 1:10),
				nrow=15, dimnames=list(character(0),
				c( "1.1", "1.2"))),
		     units = c( aunits, "cm", "ml" )))
}
{
  # test timeSeries seriesMerge -- with matchtol
  
  positions(a)@time.zone <- "PST"
  positions(b)@time.zone <- "JST"

  compare.series( seriesMerge( a, b, pos = "union",  
	matchtol = .3, localzone = TRUE ), 
	     timeSeries(pos= timeCalendar(d=c(1:16),
			   h=c(rep(16,10), rep(9,6)), zone="PST"),
			matrix( c(1:10,rep(NA,6),
				  rep(NA,6), 1:10),
				nrow=16, dimnames=list(character(0),
				c( "1.1", "1.2"))),
		     units = c( aunits, "cm", "ml" )))
}

{
  # test timeSeries align with by argument
  a <- timeSeries(pos= timeCalendar( d=1:20, h=1:20, min=15 ), 1:20 )
  x = aggregate( a, FUN=mean, by="weeks" )
  b <- align( a, by="days", k.by=2, matchtol=1 )

  compare.series( b, timeSeries( pos=timeCalendar( d=seq(1,21,by=2)),
				c( seq( 1, 11, by=2), seq( 12, 20, by=2 ))))
}


{
  # test aggregate
  x = aggregate( a, FUN=mean, by="weeks" )
  compare.series( aggregate( a, FUN=mean, by="weeks" ),
	     timeSeries( pos=timeCalendar( d=c(1,8,15)), 
	                 as.matrix(c( 4, 11, 17.5 ))))
}

{
  # test aggregate #2
  a <- timeSeries( pos=timeCalendar( d=c(rep(1,10), rep( 2,10)), 
	h=1:20, min=1:20 ), 1:20 )

  compare.series( aggregate( a, by="days", FUN=hloc ),
	     timeSeries( pos=timeCalendar( d=1:2 ),
			 matrix( c( 10, 20, 1, 11, 1, 11, 10, 20 ), nrow=2 )))
}

{
  # clean up
  # rm( a, b, x, y, alpos, apos, adat1, adat2, adat3, aunits, compare.series )
  options(save.options)
  rm(save.options)
  TRUE
}


