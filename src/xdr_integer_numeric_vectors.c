#include <Rinternals.h>


// Basic idea borrowed from serialize.c
#include <rpc/types.h>
#include <rpc/xdr.h>


SEXP
xdr_integer(SEXP r_buf)
{
    R_xlen_t len = Rf_length(r_buf);// XXX fix to Xlength.
    R_xlen_t numValues = len/4;
    void *buf;
    XDR xdrs;
    SEXP ans;
    buf = RAW(r_buf);
    PROTECT(ans = allocVector(INTSXP, numValues));

    xdrmem_create(&xdrs, buf, (int)(numValues * sizeof(int)), XDR_DECODE);
    for(int i = 0; i < numValues; i++) {
	    if(!xdr_int(&xdrs, INTEGER(ans) + i))
		error(("XDR read failed for integer/int"));
    }
    xdr_destroy(&xdrs);
    UNPROTECT(1);
    return(ans);
}


SEXP
xdr_numeric(SEXP r_buf, SEXP r_numVals) // don't need r_numVals even for complex?
{
//    R_xlen_t len = Rf_length(r_buf);// XXX fix to Xlength.
    R_xlen_t numValues = INTEGER(r_numVals)[0];  // len/8;
    void *buf;
    XDR xdrs;
    SEXP ans;
    buf = RAW(r_buf);
    PROTECT(ans = allocVector(REALSXP, numValues));

    xdrmem_create(&xdrs, buf, (int)(numValues * sizeof(double)), XDR_DECODE);
    for(int i = 0; i < numValues; i++) {
	    if(!xdr_double(&xdrs, REAL(ans) + i))
		error(("XDR read failed for numeric/double"));
    }
    xdr_destroy(&xdrs);
    UNPROTECT(1);
    return(ans);
}

    
