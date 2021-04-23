/*************************************************************************
 *
 * © 1998-2012 TIBCO Software Inc. All rights reserved. 
 * Confidential & Proprietary 
 *
 *************************************************************************/

#include "seriesFuns.h"
#include "Syms.h"

#include <R_ext/Rdynload.h>

R_NativePrimitiveArgType series_lag_args[6] = 
  {REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP};
R_CMethodDef cMethods[] = {
  {"series_lag", (DL_FUNC) &series_lag, 6, series_lag_args},
  {NULL, NULL, 0}
};

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_splusTimeSeries(DllInfo *dll)
{
  R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

/* These are callable from other packages' C code: */

#define RREGDEF(name)  R_RegisterCCallable("splusTimeSeries", #name, (DL_FUNC) name)

    splusTimeSeries_NS = R_FindNamespace(mkString("splusTimeSeries"));
    if(splusTimeSeries_NS == R_UnboundValue)
      error("missing 'splusTimeSeries' namespace: should never happen");

#ifdef DEBUG_splusTimeSeries
    if(isEnvironment(splusTimeSeries_NS))
	Rprintf("splusTimeSeries_NS: %s\n",
		CHAR(asChar(eval(lang2(install("format"),splusTimeSeries_NS),
				 R_GlobalEnv))));
    else
#else
    if(!isEnvironment(splusTimeSeries_NS))
#endif
	error("splusTimeSeries namespace not determined correctly");
}

