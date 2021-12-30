#include <Rinternals.h>


// All of this is basically from serialize.c
#include <rpc/types.h>
#include <rpc/xdr.h>

#define CHUNK_SIZE 8096

#define min2(a, b) ((a) < (b)) ? (a) : (b)


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
xdr_numeric(SEXP r_buf)
{
    R_xlen_t len = Rf_length(r_buf);// XXX fix to Xlength.
    R_xlen_t numValues = len/8;
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





    
/*    
    for (done = 0; done < len; done += this) {
	this = min2(CHUNK_SIZE, len - done);
//	stream->InBytes(stream, buf, (int)(sizeof(int) * this));
	xdrmem_create(&xdrs, buf, (int)(this * sizeof(int)), XDR_DECODE);
	for(int cnt = 0; cnt < this; cnt++)
	    if(!xdr_int(&xdrs, INTEGER(obj) + done + cnt))
		error(_("XDR read failed"));
	xdr_destroy(&xdrs);
    }
*/
