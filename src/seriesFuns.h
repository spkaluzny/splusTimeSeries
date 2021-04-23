/*************************************************************************
 *
 * © 1998-2012 TIBCO Software Inc. All rights reserved. 
 * Confidential & Proprietary 
 *
*************************************************************************/

#ifndef SERIESLIB_SERIESFUNS_H
#define SERIESLIB_SERIESFUNS_H

#include <R.h>
#include <Rdefines.h>
#include <Rversion.h>

void series_lag(double *y, Sint *nrow, Sint *ncol, Sint *p, 
		Sint *np, double *ylag);

#endif  // SERIESLIB_SERIESFUNS_H
