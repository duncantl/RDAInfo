#include <Rinternals.h>
#include <R_ext/Connections.h>

#include <rpc/types.h>
#include <rpc/xdr.h>


SEXP
R_eatCharVectorElements(SEXP rConn, SEXP r_nels)
{
    R_xlen_t nels, i;
    nels = INTEGER(r_nels)[0];
    char ibuf[4];
    char strBuf[10000];
    int nchars;

    int totalChars = 0;

    XDR xdrs;
    Rconnection con = R_GetConnection(rConn);
    for(i = 0; i < nels; i++) {
	R_ReadConnection(con, ibuf, 4);
	R_ReadConnection(con, ibuf, 4);
	xdrmem_create(&xdrs, ibuf, (int)(sizeof(int)), XDR_DECODE);
	if(!xdr_int(&xdrs, &nchars))
		error(("XDR read failed for integer/int"));

	// if nchars = -1, then NA_character_
	if(nchars != -1) { 
	    totalChars += nchars;
	    // want seek here rather than read.
	    R_ReadConnection(con, strBuf, nchars);
	    //con->seek(con, 2, 1)
	}
    }
    xdr_destroy(&xdrs);
    return(ScalarInteger(totalChars));
    //return(rConn);
}
