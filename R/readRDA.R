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

    readHeader(con)
}

readHeader =
function(con)    
{
    start = readChar(con, 5L, useBytes = TRUE)
    # gives "RDX3\n"


    #  = readBin(con, "raw", 1L)
    format = readBin(con, 'raw', 2L) # "character", 1L)
    format = rawToChar(format)
    # Using XDR format. See InInteger.

    versions = replicate(3, readInteger(con))
    # How to convert the writer_version, min_reader_version to 4.2.0 and 3.5.0

    browser()
    nc = readInteger(con)
    enc = rawToChar(readBin(con, 'raw', nc))
    list(format = trimws(format), versions = versions, encoding = enc)
    
#    nchar = readBin(con, "integer", 1L)
#    encoding = readChar(con, "integer", nchar)    
    
}

readInteger =
function(con, nel = 1L)
{
#    browser()
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
