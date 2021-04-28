\name{tbauc.3m}
\alias{tbauc.3m}
\alias{tbauc.6m}
\alias{tbauc.1y}
\docType{data}
\title{
Treasury Bill Auction Rates
}
\description{
Treasury Bill auction rate data
running from 1980 to 1997 for 3 month, 6 month, and 1 year durations.
}
\format{
Three separate \code{timeSeries} objects with Treasury Bill auction rates:

\describe{

\item{tbauc.3m}{
Average of interest rate bids accepted in regular treasury auctions
of 13-week bills (also known as 3-month bills).
Currently, the auctions are held each Monday
for bills to be issued the ensuing Thursday, in the absence
of holidays.
}
\item{tbauc.6m}{
Average of interest rate bids accepted in regular treasury auctions
of 26-week bills (also known as 6-month bills).
Currently, the auctions are held each Monday
for bills to be issued the ensuing Thursday, in the absence
of holidays.
}
\item{tbauc.1y}{
Average of interest rate bids accepted in regular treasury auctions
of 52-week bills (also known as 1-year bills).
Currently, the auctions are held at roughly
monthly intervals.
}
}
}
\source{
From the web site of the Federal Reserve Bank,
\url{https://www.federalreserve.gov/releases/h15/data.htm}.
}
\keyword{datasets}

