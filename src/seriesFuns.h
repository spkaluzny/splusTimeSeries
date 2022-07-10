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

/* Sfloat, Sint added to splusTimeSeries_1.5.5 as they will be dropped     
 * from R soon.
 */
typedef double Sfloat;
typedef int Sint;

void series_lag(double *y, Sint *nrow, Sint *ncol, Sint *p, 
		Sint *np, double *ylag);

#endif  // SERIESLIB_SERIESFUNS_H
