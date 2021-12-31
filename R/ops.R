`[[.RDAToc` =
function(x, i, j, ...)    
{
    if(!(i %in% names(x)))
        return(NULL)

    offset = unclass(x)[[i]]$offset    
        
    file = x$.meta$file
    if(is.character(file)) {
        con = gzfile(file, "rb")
        on.exit(close(con))
    } else
        con = file

    seek(con, offset)
    # Read the tag
    ReadItem(con, FALSE, hdr = x$header)
    # Read the value
    ReadItem(con, FALSE, hdr = x$header)
}

names.RDAToc =
function(x)
{
   setdiff(base::names(unclass(x)), ".meta")
}

print.RDAToc =
function(x, ...)
{
    print(x$.meta$file)
    print(x$.meta$header$native_encoding)    
    print(as.data.frame(x))
}

as.data.frame.RDAToc =
function(x, row.names = NULL, optional = FALSE, ...)
{
    vars = names(x)
    x = unclass(x)
    tmp = do.call(rbind, lapply(x[seq_len(length(x)-1)], function(x) x[, c("type", "length", "class", "offset")]))
    rownames(tmp) = vars
    tmp
}

