{
  library(splusTimeSeries)
  TRUE
}

{
  # generate hloc plot from Dow Jones data
  data("djia", package="splusTimeSeries")
  
  dow <- djia[positions(djia)>=timeDate("09/01/87") &
     positions(djia)<=timeDate("11/01/87"), 1:4]

  plot(dow, plot.type="hloc")
}

{

  # generate hloc plot from tbond data
  data("tbond", package="splusTimeSeries")
  tb <- tbond[positions(tbond)>=timeDate("02/01/94") & 
     positions(tbond)<=timeDate("02/13/94"),]

  plot(tb, plot.type="hloc")
}


{

  # generate line plot from tbill auction data
  data("tbauc.3m", package="splusTimeSeries")
  tb3m <- tbauc.3m[positions(tbauc.3m)>=timeDate("01/01/96") &
     positions(tbauc.3m)<=timeDate("07/01/97"),]
  data("tbauc.6m", package="splusTimeSeries")
  tb6m <- tbauc.6m[positions(tbauc.6m)>=timeDate("01/01/96") & 
    positions(tbauc.6m)<=timeDate("07/01/97"),]
  data("tbauc.1y", package="splusTimeSeries")
  tb1y <- tbauc.1y[positions(tbauc.1y)>=timeDate("01/01/96") & 
     positions(tbauc.1y)<=timeDate("07/01/97"),]

  plot(tb3m, tb6m, tb1y)
}

{
  # generate point plot from tbill auction data

  plot(tb3m, tb6m, tb1y, merge.args=list(pos="union"),
       plot.args=list(type="p"))
}

{
  # generate line plot from network packet data
  data("net.packet", package="splusTimeSeries")
  np <- net.packet[net.packet[,1]=="TCP",2]
  np <- np[1:1000,]

  plot(np)
}

{
  # generate moving avg plot
  tb.hloc <- aggregateSeries( tbond, pos=timeSeq( timeDate("1/7/1994" ),
					 timeDate( "2/4/1995" ), by = "days" ),
			   colnames = c( "high", "low", "open", "close" ),
			   FUN = hloc, together=TRUE )
  tb.avg <- aggregate( tb.hloc[,"close"], moving=20, FUN=mean, adj=1 )

  plout <- plot( tb.hloc, plot.type="hloc", main="T-Bonds")
  lines.render( positions(tb.avg), seriesData(tb.avg), 
	      x.scale = plout$scale, col=3 )
}

{
  # generate signal plot from say.wavelet
  data("say.wavelet", package="splusTimeSeries")

  plot( say.wavelet )
}

{
  # generate log plot from say.wavelet
  plot( say.wavelet[-1,] + 300, log.axes="xy" )
}

{
  # generate db plot from say.wavelet
  plot( say.wavelet + 300, dB=TRUE )
}
