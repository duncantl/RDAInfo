Load =
    #
    # this is a very simple layer on top of the load() function.
    # It allows the caller to load() an RDA file and restore 
    # a subset of the variables, not all of them.
    # This avoids overwriting existing variables.
    #
    # This does read and restore all of the variables in the Rda file and discards the ones you
    # don't want to keep.
    #
function(file, envir = parent.frame(), verbose = FALSE, vars = character(), ..., .noOverwrite = TRUE)
{
    vars = c(vars, unlist(list(...)))
    
    if(length(vars) == 0)
        return(invisible(load(file, envir, verbose)))

    e = new.env()
    load(file, e, verbose)

    
    new.vars = names(vars)
    if(length(new.vars) == 0) 
        names(vars) = new.vars = vars
    else if(any(w <- (names(vars) == "")))
         new.vars[w] = vars[w]        

    mapply(function(v1, v2) if(.noOverwrite && !exists(v1, envir)) assign(v1, get(v2, e), envir), new.vars, vars)
    invisible(vars)
}


