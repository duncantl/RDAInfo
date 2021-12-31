Load =
function(file, envir = parent.frame(), verbose = FALSE, vars = character(), ...)
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

    mapply(function(v1, v2) assign(v1, get(v2, e), envir), new.vars, vars)
    invisible(vars)
}


