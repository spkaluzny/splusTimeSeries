/*************************************************************************
 *
 * © 1998-2012 TIBCO Software Inc. All rights reserved. 
 * Confidential & Proprietary 
 *
 *************************************************************************/

/*************************************************************************
 * This file contains C code utility functions for series objects.
 *
 * The exported functions here were written to be called with the 
 * .Call interface of R.  They include (see documentation in func headers):

    void series_lag(double *y, Sint *nrow, Sint *ncol, Sint *p, 
		Sint *np, double *ylag);

*************************************************************************/

#include "seriesFuns.h"

/* 
 * called by lag.timeSeries and lag.signalSeries methods 
 */
void series_lag(double *y, Sint *nrow, Sint *ncol, Sint *p, Sint *np,
		double *ylag)
{
  /* 
   * function : computes lagged matrix out of y
   * parameter:
   *        y : nrow x ncol matrix. stored with y[i][j] = y[i+j*nrow]
   *        p : np x 1 vector, number of lags to be used
   *     ylag : nrow x (ncol*np) matix. lagged matrix on output.
   */
  Sint iter, i, j, nlag, colIdx1, colIdx2;
  
  for (iter=0; iter < *np; iter++) {
    nlag = p[iter];
    for (j=0; j < *ncol; j++) {
      colIdx1 = (iter * (*ncol) + j) * (*nrow);
      colIdx2 = j * (*nrow);
      if (nlag >= 0) {
        for (i=nlag; i < *nrow; i++) { 
	  ylag[i+colIdx1] = y[i-nlag+colIdx2];
        }
      }
      else {
        for (i=0; i < *nrow+nlag; i++) {
          ylag[i+colIdx1] = y[i-nlag+colIdx2];
        }
      }
    }
  }
}
