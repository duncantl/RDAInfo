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
#seek(con, offset)
    readBin(con, 'raw', offset) 

    header = attr(x, "header")
    # Read the tag
    ReadItem(con, FALSE, hdr = header)
    # Read the value
    ReadItem(con, FALSE, hdr = header)
}

`$.RDAToc` =
function(x, name)    
{
    x[[name]]
}


if(FALSE)  # don't need now with the file and header as attributes, not in an element named .meta.
names.RDAToc =
function(x)
   setdiff(base::names(unclass(x)), ".meta")


print.RDAToc =
function(x, ...)
{
    # message or cat??
    message("file: ", attr(x, "file"))
    message("encoding: ", attr(x, "header")$native_encoding, "\n")
    if(length(x) == 1)
        print(unclass(x)[[1]])
    else
        print(as.data.frame(x))
}

as.data.frame.RDAToc =
function(x, row.names = NULL, optional = FALSE, elNames = "union", ...)
{
    vars = names(x)
    x = unclass(x)  # leave as we will probably add a [.RDAToc method and then that would call that.

    if(length(elNames) == 1 ) {
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
function(df, vars)
{
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

#   if(any(duplicated(i))) 
#      structure(lapply(unique(i), function(id) x[[id]]), names = unique(i))[i]
#   else
#      structure(lapply(i, function(id) x[[id]]), names = i)        
}


# readVariables(f, c("m", "a", "d", "m"), info)
readVariables =
function(file, vars, info, hdr)
{
    uvars = unique(vars)
    offsets = sapply(unclass(info), `[[`, "offset")[uvars]
   
    off = sort(offsets)

      # Would prefer to seek, but doesn't work for gzfile()'s
    jumpTo = function(pos)
                readBin(con, 'raw', pos - seek(con))

    con = gzfile(file, "rb")
    on.exit(close(con))
    
    ans = lapply(off, function(pos) {
                         jumpTo(pos)
                         ReadItem(con, FALSE, hdr = hdr)
                         ReadItem(con, FALSE, hdr = hdr)                    
                      })
    
    ans[vars]
}



