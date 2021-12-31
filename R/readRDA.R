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
    ans = ReadItem(con, TRUE, hdr = header, depth = 0L)
    attr(ans, "file") = file
    attr(ans, "header") = header
    ans
}

ReadItem =
    # hdr is the header from readHeader() to provide context. 
function(con, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    ty = readInteger(con)
    elInfo = unpackFlags(ty)

    sexpType = sexpType(elInfo["type"])
    switch(sexpType,
           LISTSXP = readPairList(con, elInfo, skipValue, hdr),
           LGLSXP =,
           INTSXP =,
           REALSXP =, 
           CPLXSXP =,
           STRSXP = readVector(con, elInfo, skipValue, hdr), 
           VECSXP = readList(con, elInfo, skipValue, hdr),
           NILSXP = ,
           NILVALUE_SXP = NULL,
           GLOBALENV_SXP = globalenv(),
           EMPTYENV_SXP = emptyenv(),
           BASEENV_SXP = getNamespace("base"),
           CLOSXP = readFunction(con, elInfo, skipValue, hdr),
           # both MISSINGARG_SXP and UNBOUNDVALUE_SXP correspond to
           # C level SYMSXPs that are not necessarily/obviously available to use at the R level.?????
           MISSINGARG_SXP = "bob", # XXXX   quote(foo(,))[[2]],  need to mimic R_MissingArg
           # UNBOUNDVALUE_SXP  
           LANGSXP = readLangSEXP(con, elInfo, skipValue, hdr),
           SYMSXP = readTag(con, elInfo), #???
           # REFSXP
           # NAMESPACE_SXP
           # PACKAGESXP
           stop("unhandled type in ReadItem ", sexpType)
          )
}



readPairList =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    ans = list()
    positions = numeric()
    while(TRUE) {
        pos = seek(con)
        name = readTag(con)
        positions[name] = pos
        value = ReadItem(con, skipValue, hdr, depth = depth + 1L)
        ans[[name]] = value
        ty = readType(con)
        if(ty["type"] == NILVALUE_SXP || ty["type"] == NILSXP)
            break
    }

    if(depth == 0) {
#        browser()
        ans = mapply(function(d, p) { d$offset = p; d}, ans, positions, SIMPLIFY = FALSE)
        class(ans) = "RDAToc"
    }
    
    if(info["hasattr"] > 0) {
        at = readAttributes(con, skipValue, hdr, depth = depth + 1L)
        if(skipValue)
            browser()
        else
            attributes(ans) = at
    }
    ans
}


readAttributes =
function(con, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    ty = readInteger(con)  # should be a LISTSXP
    at = readPairList(con, unpackFlags(ty), hdr = hdr, depth = depth)
}


#############

readLangSEXP =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    #   readFunction(con, info, skipValue, hdr)
    k = ReadItem(con, skipValue = FALSE, hdr)
    browser()
}


readFunction =
    #  See serialize.c::1135
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    # info says there is a tag . serialize for the output explicitly sets this.
    at = NULL
    if(info['hasattr'])
        at = readAttributes(con, skipValue = FALSE, hdr = hdr)

#    tag = readTag(con)
      # env
    env = ReadItem(con, skipValue = FALSE, hdr = hdr)
    formals = ReadItem(con, skipValue = FALSE, hdr = hdr)
    body = ReadItem(con, skipValue, hdr = hdr)

    browser()
    ans = NULL
    if(!is.null(at)) {

    }

    ans
}



#############

readList =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    len = readInteger(con)
    ans = replicate(len, ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth + 1L), simplify = FALSE)
    if(info["hasattr"] > 0) {
        at = readAttributes(con, skipValue = FALSE, hdr = FALSE, depth = depth + 1L)
        #XXXX what to do with them if skipValue = true.
        attributes(ans) = at
    }

    ans
}

readVector =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    len = readInteger(con)
    ty = sexpType(info["type"])
#    browser()
    if(ty == "STRSXP") {
        #XXXX need to handle attributes on the character vector.
        return(readCharacterVector(con, len, skipValue = skipValue, hdr = hdr, depth = depth + 1L))
    }
    
    itemSize = switch(ty,
                      LGLSXP = 4L,
                      INTSXP = 4L,
                      REALSXP = 8L,
                      RAWSXP = 1L,
                      CPLXSXP = 16L)

    ans = if(skipValue) {
              if(ty == "STRSXP") 
                  readCharacterVector(con, len, skipValue, hdr = hdr, depth = depth + 1L)
              else 
                  seek(con, len * itemSize, origin = "current")

              data.frame(type = ty, length = len, class = NA, names = FALSE)
          } else {
              buf = readBin(con, 'raw', len * itemSize)
              if(ty == "CPLXSXP") {
                  tmp = .Call("xdr_numeric", buf, len*2L)
                  i = seq(1, length = len, by = 2)
                  complex(real = tmp[i], imaginary = tmp[i+1L])
              } else
                  switch(ty,
                         LGLSXP = as.logical(.Call("xdr_integer", buf)),
                         INTSXP = .Call("xdr_integer", buf),
                         REALSXP = .Call("xdr_numeric", buf, len),
                         RAWSXP = buf)
          }

    if(info['hasattr'] > 0) {
        at = readAttributes(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)
        if(skipValue) {
            if("names" %in% names(at) && length(at$names))
                ans$name = TRUE
            if("class" %in% names(at) && length(at$class))
                ans$class = at$class
            if(!all(w <- (names(at) %in% c("names", "class"))))
                warning("ignoring attribute names on vector: ", paste(names(at)[!w], collapse = ", "))
        } else
            attributes(ans) = at
    }
    
    ans
}

readCharacterVector =
function(con, len, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    # Need to know if to process attributes or leave this to readVector???
    if(skipValue) {
        #nc = replicate(len, { readInteger(con); nchar = readInteger(con); seek(con, nchar, "current"); nchar})
        nc = .Call("R_eatCharVectorElements", con, len)
        ans = data.frame(type = "STRSXP", length = len, class = NA, names = FALSE, totalNumCharacters = sum(nc))
    } else 
        replicate(len, { readInteger(con); readCharsxp(con, skipValue = skipValue, hdr = hdr)})
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


#################

readType =
function(con)    
    unpackFlags(readInteger(con))

readTag =
function(con, info = NULL)
{
    if(is.null(info)) {
        flags = readInteger(con)
        info = unpackFlags(flags)
    }

    flags = readInteger(con)
ty = unpackFlags(flags)
if(ty["type"] != 9) browser()    
    # type for this had better be 9
    readCharsxp(con)
}



# 
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
