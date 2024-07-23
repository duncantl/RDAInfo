#!!! Restore check  @745 check for type != 9 in readTag()
#library(bitops)

toc =
function(file)
{
    lload(file, TRUE)
}

lload =
function(file, skipValue = FALSE)    
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

    ans = ReadItem(con, skipValue, hdr = header, depth = 0L)
    attr(header$references, "locked") = TRUE
    
    attr(ans, "file") = file
    attr(ans, "header") = header

    ans
}

ReadItem =
    # hdr is the header from readHeader() to provide context for all of the operations,
    # specfically the native_encoding. And now the references container.
function(con, skipValue = FALSE, hdr = NULL, depth = 0L,
         ty = readInteger(con), elInfo = unpackFlags(ty, depth))    
{
# cat("ReadItem:");print(unname(elInfo))
    
    sexpType = sexpType(elInfo["type"])
#print(elInfo)
#if(depth == 0) browser()    
    switch(sexpType,
           LISTSXP = readPairList(con, elInfo, skipValue, hdr, depth = depth),
           LGLSXP =,
           INTSXP =,
           REALSXP =, 
           CPLXSXP =,
           RAWSXP =,
           STRSXP = readVector(con, elInfo, skipValue, hdr, depth),
           # Do we ever see the CHARSXP since we process them when reading the STRSXP ?
           CHARSXP = readCharsxp(con, skipValue, hdr, depth = depth),
           EXPRSXP = ,
           VECSXP = readList(con, elInfo, skipValue, hdr, depth),
           NILSXP = ,   # Won't see this as SaveSpecialHook maps to R_NilValue to NILVALUE_SXP.
           NILVALUE_SXP = NULL,
           ENVSXP = readEnvironment(con, elInfo, skipValue, hdr, depth = depth),
           GLOBALENV_SXP = if(skipValue) defaultDesc(sexpType) else globalenv(),  # may need to add this and emptyenv to reference table.
           EMPTYENV_SXP = emptyenv(),
           EXTPTRSXP = readExternalPointer(con, elInfo, skipValue, hdr, depth),
           BASEENV_SXP = ,
           BASENAMESPACE_SXP = getNamespace("base"),           # Or .BaseNamespaceEnv
           CLOSXP = readFunction(con, elInfo, skipValue, hdr, depth = depth),
           # both MISSINGARG_SXP and UNBOUNDVALUE_SXP correspond to
           # C level SYMSXPs that are not necessarily/obviously available to use at the R level.?????
           MISSINGARG_SXP = structure(TRUE, class = "MISSINGARG_SXP"), #quote(foo(,))[[2]], # need to mimic R_MissingArg       # alist(x=)
           UNBOUNDVALUE_SXP = structure(NA, class = "UnboundValue"),
           DOTSXP =,
           PROMSXP = ,
           LANGSXP = readLangSEXP(con, elInfo, skipValue, hdr, depth),
           SYMSXP = readSYMSXP(con, elInfo, hdr, depth = depth),
           SPECIALSXP =,
           BUILTINSXP = readSpecial(con, elInfo, skipValue, hdr, depth),
           REFSXP = readREFSXP(con, ty, hdr, depth),
           BCODESXP = readBCODESXP(con, elInfo, skipValue, hdr, depth),
           OBJSXP =,
           S4SXP = readS4(con, elInfo, skipValue,hdr, depth),
           ALTREP_SXP = readAltRepSXP(con, elInfo, skipValue, hdr, depth),
           NAMESPACESXP = readNamespace(con, hdr, depth),
           # PACKAGESXP
           stop("unhandled type in ReadItem ", sexpType)
          )
}

readDots =
function(con, skipValue = FALSE, hdr = NULL, depth = 1)    
{

}

readNamespace =
function(con, hdr = NULL, depth = 1)    
{
    # read the 0
    readInteger(con)
    len = readInteger(con)
    spec = readCharacterVector(con, len, skipValue = FALSE, hdr = hdr, depth = depth + 1L, hasAttr = FALSE)
    getNamespace(spec[1])
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
    ats = NULL
    if(info["hasattr"])
        ats = ReadItem(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)

    if(!skipValue)
        return(mkS4Object(ats))
    
    if(length(ats)) {
            ans = defaultDesc("S4SXP", class = as.character(ats$class), package = attr(ats$class, "package"))
            ans$slotNames = list(setdiff(names(ats), "class"))
    } else
        ans = defaultDesc("S4SXP")
    
    ans
}

mkS4Object =
function(ats)
{
    ans = new(ats$class)
    slots = slotNames(ans)
    for(i in slots) {
        slot(ans, i) = ats[[ i ]]
    }
    ans
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

Cat =
function(depth, ...)
    cat(ind(depth), ...)

ind = 
function(n)
{
    #    if(!is.numeric(n) || n < 0) stop("problem")
    # browser()
    paste(rep(" ", n), collapse = "")
}


readPairList =
    #
    # info is the c(type = 2, hasattr = , hastag = , ...)
    #
    # This is the iterative (rather than recursive) version.
    #
    # info is the result of readType() that identifies the LISTSXP for the pair list.
    # 
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    ans = list()
    positions = numeric()
    ty = info

    ats = NULL  # attributes on the pair list itself.
    ndepth = depth + 1L

    if(info['hasattr']) 
        ats = readAttributes(con, skipValue = FALSE, hdr = hdr, depth = ndepth)

    info1 = info
    
    ctr = 1L
    while(TRUE) {
        if(FALSE && depth == 0 && ctr >= 23) {
            # √ debug(ReadItem); debug(readTag); debug(readAttributes); debug(readPairList);  debug(readFunction);
            # √ debug(readLangSEXP); debug(readList); debug(readEnvironment);
            # debug(readREFSXP) 
            # debug(readSYMSXP)
            # debug(addRef)
            browser()
            # debug(readFunction)      
            # debug(readVector); debug(readCharacterVector); debug(readVectorValues);
        }
        
        tag = character()
        if(info['hastag'])  {
            tag = as.character(readTag(con, hdr = hdr, depth = ndepth))
            if(FALSE && depth == 0) {
                print(tag)
               # print(info)
            }
        }
        
        positions[ctr] = pos = seek(con)
###       if(ty["hastag"]) { 
###           name = readTag(con, hdr = hdr, depth = depth + 1L)
###             # name is likely to be a name/symbol object, so need to coerce to character.
###           positions[as.character(name)] = pos
###       } else
        name = length(ans) + 1L

        # have the type for the next element if length(ans) > 0. elInfo = ty)
        value = ReadItem(con, skipValue, hdr, depth = depth + 1L)
        if(is.null(value) && name == 1L)
            ans = list(NULL)
        else
            ans[[name]] = value
        
        if(length(tag) > 0)
            names(ans)[name] = tag

        ty = readType(con, depth)
#cat("readPairList: "); print(unname(ty))
#if(ty["type"] == 13L) browser()

        if(ty["type"] == NILVALUE_SXP || ty["type"] == NILSXP)
            break
        #XXX remove this.
        if(is.null(value))
            break
        
        info = ty
        ctr = ctr + 1L
    }

    if(!is.null(ats))
        attributes(ans) = at

    if(depth == 0 && skipValue) {
        ans = mapply(setOffset, ans, positions,
                     SIMPLIFY = FALSE)
        attr(ans, "offsets") = positions
        class(ans) = "RDAToc"
    }

    
    if(info1["hasattr"] > 0) {
        at = readAttributes(con, skipValue, hdr, depth = depth + 1L)
        if(skipValue) {
          #  browser()
        } else
            attributes(ans) = at
    }
    
    ans
}


setOffset =
function(x, offset)    
{
    w = is.name(x) || is.environment(x)  # identical(x, emptyenv()) || identical(x, .BaseNamespaceEnv) 
    if(w)
        wrap(x, typeof(x), offset = offset)
    else
        x
}


readAttributes =
function(con, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    ty = readInteger(con)  # should be a LISTSXP
#XXX  what if ty is not a LISTSXP/pairlist. Could it be a reference. 
#    if(sexpType(unpackFlags(ty, depth)["type"]) != "LISTSXP") stop("problems")
    at = readPairList(con, unpackFlags(ty, depth), hdr = hdr, depth = depth + 1L)
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
    # Note that the attributes are first.
    #
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
        # If we add hasDotsParam, paramNames and hasDefaultValue in call to defaultDesc()
        # we get as many rows as there are parameters.
        ans$hasDotsParam = "..." %in% names(formals)
        ans$paramNames = list(names(formals))
        ans$hasDefaultValue = list(sapply(formals, hasDefaultValue))
        
        if(!is.null(at)) 
            ans = addDescAttrs(ans, at)
    } else {
        ans = mkFormals(formals)
        body(ans) = body
        environment(ans) = env
        #XXX add back when we have references working correctly. Eventhough the function isn't work, the print is all wrong with the attributes (srcref) set with no reference table.
        attributes(ans) = at
    }
    
    ans
}

hasDefaultValue =
function(x)
{
  return( class(x) != "MISSINGARG_SXP") 

  # !(is.name(x) && as.character(x) == "")
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
    #XXX TEST with large vectors. (previously... may need to make this readLength() to deal with larger values.)
    len = readLENGTH(con) 
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

            ans$names = list(at$names)
            ans$elTypes = list(sapply(colInfo, `[[`, "type"))
            
            if(!is.na(class) && "data.frame" %in% class) {
                #XXX  call addDescAttrs() directly. Don't do this here.
                ans$dim = list(c(colInfo[[1]]$length, len))
                ans$hasRowNames = "row.names" %in% names(at) && length(at$row.names) > 0
            } else {
                ans$elLengths = list(sapply(colInfo, function(x) x$length))
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
                  readVectorValues(ty, con, len, itemSize)
                  # seek(con, len * itemSize, origin = "current")

              data.frame(type = ty, length = len, class = NA, names = FALSE)
          } else {
              readVectorValues(ty, con, len, itemSize)
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

readVectorValues =
function(ty, con, len, itemSize)
{
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
    #
    # In ReadItem
    #   PROTECT(s = ReadBC(ref_table, stream));
    #          InInteger()
    #          ReadBC1()
    #   SETLEVELS - but doesn't read
    #   SET_ATTRIB(  hasAttr ?  ReadItem() )
    #   R_BCVersionOK(s) & R_BytecodeExpr(s) - don't read
    # 
    #
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    # ReadBC
    len = readInteger(con)

    # if has attributes, read them
    ats = NULL
    if(info["hasattr"])
        ats = ReadItem(con, skipValue = FALSE, hdr = hdr, depth = depth + 1L)

    # ReadBC1 - code
    readBC1(con, skipValue, hdr, depth)

    #XX return a value,
    defaultDesc("BCODESXP")
}

readBC1 =
function(con, skipValue, hdr, depth)
{
    s = ReadItem(con, skipValue = TRUE, hdr = hdr, depth = depth + 1L)
    readBCConstants(con, skipValue = skipValue, hdr = hdr, depth = depth + 1L)    
}

readBCConstants =
function(con, skipValue, hdr, depth)
{
    num = readInteger(con)
    for(i in seq_len( num ) ) {
        type = readInteger(con)
        if(type == BCODESXP) {
            # ReadBC1
            readBC1(con, skipValue, hdr, depth + 1L)
            # u1 = ReadItem(con, skipValue, hdr = hdr, depth = depth + 1L)
            # const = readBCConstants(con, skipValue, hdr, depth = depth + 1L)
        } else if(type %in% c(LANGSXP, LISTSXP, BCREPDEF, BCREPREF, ATTRLANGSXP, ATTRLISTSXP)) {
            # ReadBCLang
            readBCLang(type, skipValue, con, hdr, depth = depth + 1L)
        } else {
            ReadItem(con, skipValue, hdr = hdr, depth = depth + 1L)
        }
    }
    NULL
}

readBCLang =
function(type, skipValue, con, hdr, depth)
{
    if(type == BCREPREF) {
        readInteger(con)
    } else if(type %in% c(BCREPDEF, LANGSXP, LISTSXP, ATTRLANGSXP, ATTRLISTSXP)) {
        if(type == BCREPDEF) {
            pos = readInteger(con)
            type = readInteger(con)
        }

        ats = NULL
        if(type %in% c(ATTRLANGSXP, ATTRLISTSXP))
             #attributes
            ats = ReadItem(con, FALSE, hdr = hdr, depth = depth + 1L)

        tag = ReadItem(con, TRUE, hdr = hdr, depth = depth + 1L)

        readBCLang(readInteger(con), skipValue, con, hdr, depth = depth + 1L)
        readBCLang(readInteger(con), skipValue, con, hdr, depth = depth + 1L)        
        
    } else
        ReadItem(con, FALSE, hdr = hdr, depth = depth + 1L)
}


#############

readAltRepSXP =
function(con, info, skipValue = FALSE, hdr = NULL, depth = 0L)    
{
    cat("AltRepSXP\n")
#    browser()
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
#    browser()

    # serialize.c calls AddReadRef() after allocating the externalptr.
    
    prot = ReadItem(con, skipValue, hdr, depth + 1L)
    tag = ReadItem(con, skipValue, hdr, depth + 1L)    

    # attributes. Should be on the externalptr.    
    ats = NULL
    if(info["hasattr"])
        # Read the attribute values, don't skip the values.
        ats = ReadItem(con, FALSE, hdr = hdr, depth = depth + 1L)
    if(skipValue) {
        ans = defaultDesc("EXTPTRSXP")
        if(length(ats)) {
            ans$attrNames = list(names(ats))
            ans$attrLengths = list(sapply(ats, length)) # function(x) x$length))
        }
    } else {
        ans = new("externalptr")
        if(length(ats))
            attributes(ans) = c(ats, attributes(ans))        
    }

    
    ans
}

#################

readType =
    #
    # Read the type of the next object and decompose the number into
    # different the flags describing the object.
    #
    # Only called in one place in the code.
    #
function(con, depth)    
    unpackFlags(readInteger(con), depth)

readTag =
    #
    # read a TAG/name/symbol as an element of a pairlist
    #
function(con, hdr, info = NULL, depth = 0L)
{
    if(is.null(info)) {
        flags = readInteger(con)
        info = unpackFlags(flags, depth)
    }

    if(info['type'] == REFSXP) 
        return(readREFSXP(con, flags, hdr, depth = depth + 1L))
    
    flags = readInteger(con)

#Sanity check.  Leave for now.  Make assert() that we can disable as a no-op.
#XXX uncomment
    #if(unpackFlags(flags, depth)["type"] != 9) recover()    

    val = as.name(readCharsxp(con, depth = depth + 1L))
    addRef(val, hdr$references)
    val
}

readREFSXP =
function(con, flags, hdr, depth = 0L)    
{
    ref = bitShiftR(flags, 8)
    if(ref == 0)
        ref = readInteger(ref)

    id = as.character(ref - 1L)
    if(exists(id, hdr$references))
        get(id, hdr$references)
    else
        NULL
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
HAS_ATTR_BIT_MASK  =   bitShiftL(1, 9)
HAS_TAG_BIT_MASK   =   bitShiftL(1, 10)


unpackFlags =
    #
    #  we don't convert the isobj, hasattr, hastag to LOGICALs
    #  This is so that we can combine all the fields/flags in a single vector.
    #  So these values may be > 1.  All we care is if 0 or not 0.
function(val, depth = -1)    
{
   ans = c(type = bitAnd(val, 255L),
           levels = bitShiftR(val,  12L),
           isobj = bitAnd(val, IS_OBJECT_BIT_MASK),
           hasattr = bitAnd(val, HAS_ATTR_BIT_MASK),
           hastag = bitAnd(val, HAS_TAG_BIT_MASK),
           depth = depth)
#print(unname(ans)) #XXX
   ans
}

rdaHeader = readHeader =
    #
    # Read the header of the RDA file.
    #
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
        stop(mkError(paste0(format,  ", not XDR format"), c("NonXDRFormat", "WrongFormat")))

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

mkError =
function(message, class = character(), call = NULL)    
{
    e = simpleError(message, call)
    class(e) = c(class, class(e))
    e
}

decodeVersion =
    #
    # See DecodeVersion in serialize.c (#2160)
    #
    # StackOverflow for % as bit operation.
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



#XX can remove after we finish deugging.
rmAttrs = 
function(x)
{
    attributes(x) = NULL
    x
}




setTraces =
function()
{
    trace(ReadItem, quote(Cat(depth, "<ReadItem>", sexpType, "\n")), at = 3,
          exit = quote(Cat(depth, "</ReadItem>\n")), print = FALSE)
    trace(readAttributes, quote(Cat(depth, "<readAttributes>", sexpType(unpackFlags(ty, depth)["type"]), "\n")), at = 3,
          exit = quote(Cat(depth, "</readAttributes>\n")), print = FALSE)
    trace(readTag, quote(Cat(depth, "<readTag>", sexpType(info["type"]), " ")),
          exit = quote(Cat(0, returnValue(), "</readTag>\n")), print = FALSE)
    trace(readPairList, quote(Cat(depth, "<readPairList>\n")), exit = quote(Cat(depth, "</readPairList>", length(ans), "\n")), print = FALSE)
    trace(readFunction, quote(Cat(depth, "<readFunction>\n")), exit = quote(Cat(depth, "</readFunction>\n")), print = FALSE)
    trace(readList, quote(Cat(depth, "<readList>\n")), exit = quote(Cat(depth, "</readList>\n")), print = FALSE)
    trace(readEnvironment, quote(Cat(depth, "<readEnvironment>\n")), exit = quote(Cat(depth, "</readEnvironment>\n")), print = FALSE)
    trace(readLangSEXP, quote(Cat(depth, "<readLangSEXP>\n")), exit = quote(Cat(depth, "</readLangSEXP>\n")), print = FALSE)
    trace(readREFSXP, quote(Cat(depth, "<readREFSXP>\n")), exit = quote(Cat(depth, "</readREFSXP>\n")), print = FALSE)
    trace(readSYMSXP, quote(Cat(depth, "<readSYMSXP>\n")), exit = quote(Cat(depth, "</readSYMSXP>\n")), print = FALSE)
}
