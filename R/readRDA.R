#library(bitops)

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

    header$references = new.env(parent = emptyenv())

    ans = ReadItem(con, TRUE, hdr = header, depth = 0L)
    attr(header$references, "locked") = TRUE
    
    attr(ans, "file") = file
    attr(ans, "header") = header

    ans
}

ReadItem =
    # hdr is the header from readHeader() to provide context for all of the operations,
    # specfically the native_encoding. And now the references container.
function(con, skipValue = FALSE, hdr = NULL, depth = 0L,
         ty = readInteger(con), elInfo = unpackFlags(ty))    
{
cat("ReadItem:");print(unname(elInfo))
    
    sexpType = sexpType(elInfo["type"])
    switch(sexpType,
           LISTSXP = readPairList(con, elInfo, skipValue, hdr, depth = depth),
           LGLSXP =,
           INTSXP =,
           REALSXP =, 
           CPLXSXP =,
           STRSXP = readVector(con, elInfo, skipValue, hdr, depth),
           CHARSXP = readCharsxp(con, skipValue, hdr, depth = depth),
           EXPRSXP = ,
           VECSXP = readList(con, elInfo, skipValue, hdr, depth),
           NILSXP = ,
           NILVALUE_SXP = NULL,
           ENVSXP = readEnvironment(con, elInfo, skipValue, hdr, depth = depth),
           GLOBALENV_SXP = if(skipValue) defaultDesc(sexpType) else globalenv(),  # may need to add this and emptyenv to reference table.
           EMPTYENV_SXP = emptyenv(),
           EXTPTRSXP = readExternalPointer(con, elInfo, skipValue, hdr, depth),
           BASEENV_SXP = ,
           BASENAMESPACE_SXP = getNamespace("base"),           
           CLOSXP = readFunction(con, elInfo, skipValue, hdr, depth = depth),
           # both MISSINGARG_SXP and UNBOUNDVALUE_SXP correspond to
           # C level SYMSXPs that are not necessarily/obviously available to use at the R level.?????
           MISSINGARG_SXP = structure(TRUE, class = "MISSINGARG_SXP"), #quote(foo(,))[[2]], # need to mimic R_MissingArg       # alist(x=)
           # UNBOUNDVALUE_SXP  
           LANGSXP = readLangSEXP(con, elInfo, skipValue, hdr, depth),
           SYMSXP = readSYMSXP(con, elInfo, hdr, depth = depth),
           SPECIALSXP=,
           BUILTINSXP = readSpecial(con, elInfo, skipValue, hdr, depth),
           REFSXP = readREFSXP(con, ty, hdr),
           BCODESXP = readBCODESXP(con, elInfo, skipValue, hdr, depth),
           S4SXP = readS4(con, elInfo, skipValue,hdr, depth),
           ALTREP_SXP = readAltRepSXP(con, elInfo, skipValue, hdr, depth),
           # NAMESPACE_SXP
           # PACKAGESXP
           stop("unhandled type in ReadItem ", sexpType)
          )
}

readSYMSXP =
function(con, elInfo, hdr, depth = 0L)    
{
    as.name(readTag(con, hdr, elInfo, depth = depth + 1L))
}

#########

readS4 =    
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
 browser()

}
    
############

defaultDesc =
function(type, length = NA, class = NA, names = NA, ...)
{
    ans = data.frame(type = type, length = length, class = class, ...)
    if(length(names) > 1)
        ans$names = list(names)
    ans
}


addDescAttrs =
function(desc, at, all = TRUE)
{
    ats = names(at)
    if(length(ats)) {
        if("names" %in% names(at) && length(at$names)) {
                desc$name = TRUE
                if(length(at$names) > 1) {
                    desc$name = list(at$names)
                } else
                    desc$name = at$names
        }
        if("class" %in% names(at) && length(at$class))
                desc$class = at$class  # XXX what about length > 1 vectors.

        
        if(!all(w <- (names(at) %in% c("names", "class")))) {
            if(all) {
                vars = names(at)[!w]
                for(i in vars) {
                    if(length(at[[i]]) > 0)
                        desc[[i]] = if(length(at[[i]]) > 1) list(at[[i]]) else at[[i]]
                }
                
            } else
                warning("ignoring attribute names on vector: ", paste(names(at)[!w], collapse = ", "))
        }
        
    }
    
    desc
}



###########

readEnvironment =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    locked = readInteger(con)
    if(skipValue)
        ans = defaultDesc("ENVSXP")
    else
        ans = new.env()

     # storing the id in case we need to update the value in hdr$references.
    id = addRef(ans, hdr$references)
    
    enclos = ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth)
    frame = ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth)
    hashtab = ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth)
    at = ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth)

    nonNull = !sapply(hashtab, is.null)
    hashtab = hashtab[nonNull]
    
    if(skipValue) {
        ans$locked = locked
        ans$enclos = enclos
        ans$frame = frame
        ans$length = length(hashtab)
        ans$names  = list(names(hashtab))
        ans$allocatedLength = length(nonNull)
    } else {
        if(identical(enclos,  globalenv()))
            parent.env(ans) = globalenv()
        else if(is.environment(enclos))
            parent.env(ans) = globalenv()
        else
            stop("fix this")
  
        attributes(ans) =  at
        
        # What to do with the frame?
        
        ##XXX check this works generally
        lapply(hashtab, function(x) assign(names(x), x[[1]], ans))
    }
    
    ans
}


readPairList =
    #
    # info is the c(type = 2, hasattr = , hastag = , ...)
    #
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    ans = list()
    positions = numeric()
    ty = info

    ats = NULL
    tags = character()
    ndepth = depth + 1L
    if(info['hasattr']) 
        ats = readAttributes(con, skipValue = FALSE, hdr = hdr, depth = ndepth)
    if(info['hastag'])
        tags = as.character(readTag(con, hdr = hdr, depth = ndepth))
    
    #browser()
    ctr = 1L
    while(ctr < 3L) {
        positions[ctr] = pos = seek(con)
###       if(ty["hastag"]) { 
###           name = readTag(con, hdr = hdr, depth = depth + 1L)
###             # name is likely to be a name/symbol object, so need to coerce to character.
###           positions[as.character(name)] = pos
###       } else
        name = length(ans) + 1L

        value = ReadItem(con, skipValue, hdr, depth = depth + 1L) # have the type for the next element if length(ans) > 0. elInfo = ty)
        ans[[name]] = value

#        ty = readType(con)
#  cat("readPairList: "); print(unname(ty))
 #if(ty["type"] == 13L) browser()

#        if(ty["type"] == NILVALUE_SXP || ty["type"] == NILSXP)
        if(is.null(value))
            break
        ctr = ctr + 1L
    }

    if(!is.null(ats))
        attributes(ans) = at


    if(depth == 0 && skipValue) {
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


    names(ans)[1] = tags    
# return(list(ans, tags = tags))  #   
    
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
    at = NULL
    tag = NULL
    if(info["hasattr"]) 
        at = ReadItem(con, skipValue = FALSE, hdr, depth = depth + 1L)
    if(info["hastag"])
        tag = ReadItem(con, skipValue = FALSE, hdr, depth = depth + 1L)

    car = ReadItem(con, skipValue = skipValue, hdr, depth = depth + 1L)
    cdr = ReadItem(con, skipValue = skipValue, hdr, depth = depth + 1L)

    if(skipValue) {
        ans = defaultDesc("LANGSXP")
        ans = addDescAttrs(ans, at)
        
    } else {
         #XXX only if car is a name, not a call.
        ans = if(is.name(car))
                  call(as.character(car))
              else
                  substitute(f(), list(f = car))
        if(length(cdr))
            ans[seq(along.with = cdr) + 1L] = cdr

        if(length(at))
            attributes(ans) = at
    }
    ans
}


readFunction =
    #  See serialize.c::1135
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    # info says there is a tag . serialize for the output explicitly sets this.   What does this mean???????
    at = NULL
    if(info['hasattr'])
        at = readAttributes(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)


    env = ReadItem(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)
    formals = ReadItem(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)
    body = ReadItem(con, skipValue, hdr = hdr, depth = depth + 1L)

    if(skipValue) {
        ans = defaultDesc("CLOSXP", numParams = length(formals))
        if(!is.null(at)) 
            ans = addDescAttrs(ans, at)
    } else {
        ans = mkFormals(formals)
        body(ans) = body
        environment(ans) = env
        #XXX add back when we have references working correctly. Eventhough the function isn't work, the print is all wrong with the attributes (srcref) set with no reference table.
        #attributes(ans) = at
    }
    
    ans
}

mkFormals =
function(parms, fun = function(){})
{
    formals(fun) = rep(alist(x=), length(parms))
    names(formals(fun)) = names(parms)
    hasDefault = !sapply(parms, inherits, "MISSINGARG_SXP")
    formals(fun)[ which(hasDefault) ] = parms[hasDefault]
    fun
}

###########

readSpecial =
function(con, elInfo, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    nc = readInteger(con)
    str = rawToChar(readBin(con, 'raw', nc))     # OR readChar() ?
    # ??? We could map this the name of the special/builtin by reading the R_FunTab via Rllvm offline and having this available.
    #  Is that what the code in serialize is doing???
    if(skipValue)
        defaultDesc(sexpType(elInfo['type']), id = str)
    else        
        get(str, getNamespace("base")) #????  is this always appropriate?
}

    

#############

readLENGTH =
function(con)    
{
    len = readInteger(con)
    if(len == -1) {
        lens = replicate(2, readInteger(con))
        bitShiftL(lens[1], 32) + lens[2]
    } else
        len
}

readList =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    len = readLENGTH(con)      #XXX TEST with large vectors. (previously... may need to make this readLength() to deal with larger values.)

    ans = replicate(len, ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth + 1L), simplify = FALSE)
    at = NULL
    if(info["hasattr"] > 0) {
        at = readAttributes(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)
        #XXXX what to do with them if skipValue = TRUE.

        if(!skipValue)
            attributes(ans) = at
    }

    if(info['type'] == ENVSXP)
        as.expression(ans)
    else {
        if(skipValue) {
            class = NA
            if(!is.null(at) && "class" %in% names(at))
                class = at$class

            colInfo = ans
            ans = defaultDesc(sexpType(info['type']), length = len, class = class)
            if(!is.na(class) && "data.frame" %in% class) {
                #XXX  call addDescAttrs() directly. Don't do this here.
                ans$names = list(at$names)
                ans$dim = list(c(colInfo[[1]]$length, len))
                ans$hasRowNames = "row.names" %in% names(at) && length(at$row.names) > 0
                ans$colInfo = list(sapply(colInfo, `[[`, "type"))
            }
        }
        
        ans
    }
    
}

readVector =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    len = readInteger(con)
    ty = sexpType(info["type"])

    if(ty == "STRSXP") {
        #XXXX need to handle attributes on the character vector.
        return(readCharacterVector(con, len, skipValue = skipValue, hdr = hdr, depth = depth + 1L, hasAttr = info['hasattr'] > 0))
    }
  
    itemSize = switch(ty,
                      LGLSXP = 4L,
                      INTSXP = 4L,
                      REALSXP = 8L,
                      RAWSXP = 1L,
                      CPLXSXP = 16L)

    ans = if(skipValue) {
              if(ty == "STRSXP")  # Isn't this handled above???
                  readCharacterVector(con, len, skipValue, hdr = hdr, depth = depth + 1L, hasAttr = info['hasattr'] > 0)
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

        if(skipValue) 
            ans = addDescAttrs(ans, at)
        else
            attributes(ans) = at
    }
    
    ans
}

readCharacterVector =
function(con, len, skipValue = FALSE, hdr = NULL, depth = 0L, hasAttr = FALSE)    
{
    # Need to know if to process attributes or leave this to readVector???
    if(skipValue) {
        #nc = replicate(len, { readInteger(con); nchar = readInteger(con); seek(con, nchar, "current"); nchar})
        nc = .Call("R_eatCharVectorElements", con, len)
        ans = data.frame(type = "STRSXP", length = len, class = NA, names = FALSE, totalNumCharacters = sum(nc))
    } else {
        #XXX this will be very slow
        ans = replicate(len, { readInteger(con); readCharsxp(con, skipValue = skipValue, hdr = hdr, depth = depth + 1L)})
    }

    if(hasAttr) {
        at = readAttributes(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)
        if(skipValue) {
            ans = addDescAttrs(ans, at)
        } else 
            attributes(ans) = at

    }

    ans
    
}

readCharsxp =
    #
    #
    #  The encoding is in the levs flag.
    #
    #
function(con, skipValue = FALSE, hdr = NULL, depth = 0L)
{
    nc = readInteger(con)
    if(skipValue) {
        seek(con, nc, "current")
        "skipped CHARSXP"
    } else {
        if(nc == -1)
            NA_character_
        else {
            chars = readBin(con, 'raw', nc)
            rawToChar(chars)
        }
        # Use the encoding in hdr.
    }
}


############

readBCODESXP =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    stop("BCODESXP in ", summary(con)$description)
#    browser()
    len = readInteger(con)
    code = ReadItem(con, skipValue = skipValue, hdr = hdr, depth + 1L)

    len2 = readInteger()
}


readAltRepSXP =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    cat("AltRepSXP\n")
    browser()
    info = ReadItem(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)  # put skipValue back in?
    state = ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth + 1L)
    attr = ReadItem(con, skipValue = skipValue, hdr = hdr, depth = depth + 1L)
    if(skipValue) {
        ans = defaultDesc("ALTREP_SXP")
        addDescAttr(ans, attr)
    } else {
        warning("not restoring ALTREP_SXP for now. returning NULL.")
        ans = NULL
    }

    ans
}

#########

readExternalPointer =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    prot = ReadItem(con, skipValue, hdr, depth + 1L)
    tag = ReadItem(con, skipValue, hdr, depth + 1L)    
    
    # ???? attributes. Should be on the externalptr.
    if(skipValue)
        defaultDesc("EXTPTRSXP")
    else {
      ans = new("externalptr")
    }
}

#################

readType =
function(con)    
    unpackFlags(readInteger(con))

readTag =
function(con, hdr, info = NULL, depth = 0L)
{
    if(is.null(info)) {
        flags = readInteger(con)
        info = unpackFlags(flags)
    }

    if(info['type'] == REFSXP) 
        return(readREFSXP(con, flags, hdr))
    
    flags = readInteger(con)

#Sanity check.  Leave for now.  Make assert() that we can disable as a no-op.
if(unpackFlags(flags)["type"] != 9) recover()    

    val = as.name(readCharsxp(con, depth = depth + 1L))
    addRef(val, hdr$references)
    val
}

readREFSXP =
function(con, flags, hdr)    
{
    ref = bitShiftR(flags, 8)
    if(ref == 0)
        ref = readInteger(ref)

    return(get(as.character(ref - 1L), hdr$references))
}

addRef = 
function(val, env)
{
    if(!is.null(v <- attr(env, "locked")) && v)
        return(NULL)
    
    count = length(ls(env))
    id = as.character(count)
    assign(id, val, env)
    id
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
    #  we don't convert the isobj, hasattr, hastag to LOGICALs
    #  This is so that we can combine all the fields/flags in a single vector.
    #  So these values may be > 1.  All we care is if 0 or not 0.
function(val)    
    c(type = bitAnd(val, 255L),
      levels = bitShiftR(val,  12L),
      isobj = bitAnd(val, IS_OBJECT_BIT_MASK),
      hasattr = bitAnd(val, HAS_ATTR_BIT_MASK),
      hastag = bitAnd(val, HAS_TAG_BIT_MASK))


readHeader =
function(con)    
{
    if(is.character(con)) {
        con = gzfile(con, "rb")
        on.exit(close(con))
    }
    
    start = readChar(con, 5L, useBytes = TRUE)

    format = readBin(con, 'raw', 2L) 
    format = trimws(rawToChar(format))
    if(format != "X")
        stop("not XDR format")


    info = list(format = format)
    
    versions = replicate(3, readInteger(con))
    info$version = versions[1]
    info[c("writer_version", "min_reader_version")] = lapply(versions[2:3], decodeVersion)

    if(info$version[1] >= 3) {
        # Get the encoding
        nc = readInteger(con)
        info$native_encoding = rawToChar(readBin(con, 'raw', nc))
    } else {
        info$native_encoding = NA
        warning("RDA format is version ", info$version[1])
    }
    
    info
}

decodeVersion =
    #
    # See DecodeVersion in serialize.c
    #
    # SO for % as bit operation.
    #  https://stackoverflow.com/questions/3072665/bitwise-and-in-place-of-modulus-operator
    #
function(val, asString = FALSE)    
{
    ver = integer(3)
    ver[1] = bitShiftR(val, 16)
    val = bitAnd(val, 2^16 - 1L)
    ver[2] = bitShiftR(val, 8)
    ver[3] = bitAnd(val, 2^8 - 1L)

    if(asString)
        paste(ver, collapse = ".")
    else
        ver
}
