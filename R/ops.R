`[[.RDAToc` =  
    #
    # Puzzles for reader
    #  if i has length > 1, do this hierarchically
    #
    #
function(x, i, j, ...)    
{
    if(is.numeric(i)) {
        if(length(i) > length(x))
            stop("index out of range")
        
        i = names(x)[i]
    } else if(!(i %in% names(x)))
        return(NULL)

    offset = unclass(x)[[i]]$offset    
        
    file = attr(x, "file")
    if(is.character(file)) {
        con = gzfile(file, "rb")
        on.exit(close(con))
    } else
        con = file

# Seek on compressed files is not reliable, so we read from the beginning
# seek(con, offset)
    readBin(con, 'raw', offset) 

    header = attr(x, "header")
    # Read the value
    ReadItem(con, FALSE, hdr = header)
}

`$.RDAToc` =
function(x, name)    
    x[[name]]

print.RDAToc =
function(x, ...)
{
    # message or cat??
    message("file: ", attr(x, "file"))
    message("encoding: ", attr(x, "header")$native_encoding, "\n")
    print(as.data.frame(x))
}

as.data.frame.RDAToc =
function(x, row.names = NULL, optional = FALSE, elNames = "union", ...)
{
    vars = names(x)
    x = unclass(x)  # leave as we will probably add a [.RDAToc method and then that would call that.

    if(length(elNames) == 1 ) {
        # These could be actual names of fields.
        # Also, could be character vector of length 1 that isn't these identifying a single field.
        # That's fine. It will be left as is.
        if(elNames == "union")
            elNames = unique(unlist(lapply(x, names)))
        else if(elNames == "intersect") {
            tt = table(unlist(lapply(x, names)))
            elNames = names(tt) [ tt == length(x) ]
        }
    }
    
    tmp = do.call(rbind, lapply(x, getVars, elNames)) # c("type", "length", "class", "offset")
    rownames(tmp) = vars
    tmp
}

getVars =
    #
    # given a data.frame, extract the variables given by vars
    # adding them if they don't exist, giving them values of NA.
function(df, vars)
{
    if(is.null(df)) {
        tmp = as.data.frame( structure(replicate(length(vars), NA, FALSE), names = vars))
        if("class" %in% vars)
            tmp[1, "class"] = "NULL"
        if("length" %in% vars)        
            tmp[1, "length"] = 0L
        return(tmp)
    }
    
    
    w = vars %in% names(df) 
    ans = df[, vars[w]]
    ans[ vars[!w] ] = NA
    ans[, vars]
}
        

`[.RDAToc` =
function(x, i, j, ...)
{
    if(!missing(j))
        stop("a second dimension doesn't make sense for an RDAToc")

    if(is.numeric(i)) { 
        if(any(i < 0))
            i = setdiff(seq(along.with = x), abs(i))
        if(any(i > length(x)))
            stop("asking for element beyond the number of elements in the rda file")
        
        i = names(x)[i]
    } else {
        i = as.character(i)
        if(!all(w <- (i %in% names(x))))
            stop( paste(i[!w], collapse = ", "), " not in rda file")
    }

    return(readVariables(attr(x, "file"), i, x, attr(x, "header")))
}


readVariables =
# readVariables(f, c("m", "a", "d", "m"), info)
function(file, vars, info, hdr)
{
    uvars = unique(vars)
    offsets = sapply(unclass(info), `[[`, "offset")

    if(!all(w <- (vars  %in% names(offsets))))
        stop("no variables named ", paste(unique(vars[!w]), collapse = ", "), " in ", f)

    off = sort(offsets[uvars])

      # Would prefer to seek, but doesn't work for gzfile()'s
    jumpTo = function(pos)
                readBin(con, 'raw', pos - seek(con))

    con = gzfile(file, "rb")
    on.exit(close(con))
    
    ans = lapply(off, function(pos) {
                         jumpTo(pos)
                         ReadItem(con, FALSE, hdr = hdr) # variable name
                         ReadItem(con, FALSE, hdr = hdr) # object    
                      })
    
    ans[vars]
}



