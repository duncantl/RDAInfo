library(bitops)

toc =
function(file)
{
    #Most of this is so similar to load
    if(is.character(file)) {
        con = gzfile(file, "rb")
        on.exit(close(con))
    } else if(inherits(file, "connection"))
        con = file
    else
        stop("invalid value of file")

    header = readHeader(con)
    ReadItem(con, hdr = header)
}

ReadItem =
    # hdr is the header from readHeader() to provide context. 
function(con, skipValue = FALSE, hdr = NULL)    
{
    ty = readInteger(con)
    elInfo = unpackFlags(ty)

    sexpType = sexpType(elInfo["type"])
    switch(sexpType,
           LISTSXP = readPairList(con, elInfo, skipValue, hdr),
           LGLSXP =,
           INTSXP =,
           REALSXP = readVector(con, elInfo, skipValue, hdr),
           STRSXP = readVector(con, elInfo, skipValue, hdr),
           VECSXP = readList(con, elInfo, skipValue, hdr),
           NILVALUE_SXP = NULL,
           stop("unhandled type in ReadItem ", sexpType)
          )
}

readList =
function(con, info, skipValue = FALSE, hdr = NULL)    
{
    len = readInteger(con)
    ans = replicate(len, ReadItem(con, skipValue = skipValue, hdr = hdr), simplify = FALSE)
    if(info["hasattr"] > 0) {
        at = readAttributes(con, skipValue = FALSE, hdr = FALSE)
        attributes(ans) = at
    }
    
#    browser()
    ans
}

readVector =
function(con, info, skipValue = FALSE, hdr = NULL)    
{
    len = readInteger(con)
    ty = sexpType(info["type"])
    if(ty == "STRSXP") {
        #XXXX need to handle attributes on the character vector.
        return(readCharacterVector(con, len, hdr = hdr))
    }
    
    itemSize = switch(ty,
                      LGLSXP = 4L,
                      INTSXP = 4L,
                      REALSXP = 8L,
                      RAWSXP = 1L)
    
    ans = if(skipValue) {
              if(ty == "STRSXP") 
                  readCharacterVector(con, len, skipValue, hdr = hdr)
              else 
                  seek(con, len * itemSize, origin = "current")

          } else {
              buf = readBin(con, 'raw', len * itemSize)
              switch(ty,
                     LGLSXP = as.logical(.Call("xdr_integer", buf)),
                     INTSXP = .Call("xdr_integer", buf),
                     REALSXP = .Call("xdr_numeric", buf),
                     RAWSXP = buf)
          }

    if(info['hasattr'] > 0) {
        at = readAttributes(con, skipValue = FALSE, hdr = hdr)
        if(skipValue) {
            browser()
        } else
            attributes(ans) = at
    }
    
    ans
}

readCharacterVector =
function(con, len, skipValue = FALSE, hdr = NULL)    
{
    replicate(len, { readInteger(con); readCharsxp(con, skipValue = skipValue, hdr = hdr)})
}

readType =
function(con)    
    unpackFlags(readInteger(con))

readPairList =
function(con, info, skipValue = FALSE, hdr = NULL)    
{
    ans = list()
    while(TRUE) {
        name = readTag(con) # , info)
        value = ReadItem(con, skipValue, hdr)
        ans[[name]] = value
        ty = readType(con)
        if(ty["type"] == NILVALUE_SXP)
            break
    }
    

    if(info["hasattr"] > 0) {
        at = readAttributes(con, skipValue, hdr)
        if(skipValue)
            browser()
        else
            attributes(value) = at
    }
    ans
}


readAttributes =
function(con, skipValue = FALSE, hdr = NULL)    
{
    ty = readInteger(con)  # should be a LISTSXP
    at = readPairList(con, unpackFlags(ty), hdr = hdr)
}


readTag =
function(con, info = NULL)
{
    if(is.null(info)) {
        flags = readInteger(con)
        info = unpackFlags(flags)
    }

    flags = readInteger(con)
    # type for this had better be 9
    readCharsxp(con)
}

readCharsxp =
function(con, skipValue = FALSE, hdr = NULL)
{
    nc = readInteger(con)
    if(skipValue) {
        seek(con, nc, "current")
    } else {
        chars = readBin(con, 'raw', nc)
        rawToChar(chars)
        # Use the encoding in hdr.
    }
}



readHeader =
function(con)    
{
    start = readChar(con, 5L, useBytes = TRUE)
    # gives "RDX3\n"

    format = readBin(con, 'raw', 2L) 
    format = trimws(rawToChar(format))
    if(format != "X")
        stop("not XDR format")


    info = list(format = format)
    
    versions = replicate(3, readInteger(con))
    info$version = versions[1]
    info[c("writer_version", "min_reader_version")] = lapply(versions[2:3], decodeVersion)

    # Get the encoding
    nc = readInteger(con)
    info$native_encoding = rawToChar(readBin(con, 'raw', nc))
    
    info
}

readInteger =
function(con, nel = 1L)
{
    buf = readBin(con, "raw", nel * 4L)
    .Call("xdr_integer", buf)
}

IS_OBJECT_BIT_MASK =   bitShiftL(1, 8)
HAS_ATTR_BIT_MASK =    bitShiftL(1, 9)
HAS_TAG_BIT_MASK =     bitShiftL(1, 10)

unpackFlags =
    #
    #  we don't conver the isobj, hasattr, hastag to LOGICALs
    #  so that we can combine all in a vector.
    #  These may be > 1  All we care is if 0 or not 0.
function(val)    
    c(type = bitAnd(val, 255L),
      levels = bitShiftR(val,  12L),
      isobj = bitAnd(val, IS_OBJECT_BIT_MASK),
      hasattr = bitAnd(val, HAS_ATTR_BIT_MASK),
      hastag = bitAnd(val, HAS_TAG_BIT_MASK))



decodeVersion =
    #
    # See DecodeVersion in serialize.c
    #
    # SO for % as bit operation.
    #  https://stackoverflow.com/questions/3072665/bitwise-and-in-place-of-modulus-operator
    #
function(val)    
{
    ver = integer(3)
    ver[1] = bitShiftR(val, 16)
    val = bitAnd(val, 2^16 - 1L)
    ver[2] = bitShiftR(val, 8)
    ver[3] = bitAnd(val, 2^8 - 1L)

    ver
}
