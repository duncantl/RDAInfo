`[[.RDAToc` =
    #
    # Puzzles for reader
    #  if i has length > 1, do this hierarchically
    #
    #
function(x, i, j, ...)    
{
    if(!(i %in% names(x)))
        return(NULL)

    offset = unclass(x)[[i]]$offset    
        
    file = attr(x, "file")
    if(is.character(file)) {
        con = gzfile(file, "rb")
        on.exit(close(con))
    } else
        con = file

    seek(con, offset)

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
    print(as.data.frame(x))
}

as.data.frame.RDAToc =
function(x, row.names = NULL, optional = FALSE, ...)
{
    vars = names(x)
    x = unclass(x)  # leave as we will probably add a [.RDAToc method and then that would call that.
    tmp = do.call(rbind, lapply(x, function(x) x[, c("type", "length", "class", "offset")]))
    rownames(tmp) = vars
    tmp
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


    if(any(duplicated(i))) 
       structure(lapply(unique(i), function(id) x[[id]]), names = unique(i))[i]
    else
       structure(lapply(i, function(id) x[[id]]), names = i)    
}