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
function(file, envir = parent.frame(), verbose = FALSE, vars = character(), .noOverwrite = TRUE)
{
    # vars = c(vars, unlist(list(...)))
    
    if(length(vars) == 0)
        return(invisible(load(file, envir, verbose)))

    e = new.env(parent = emptyenv())
    load(file, e, verbose)

    inRDA = sapply(vars, exists, e, inherits = FALSE)
    if(any(!inRDA)) {
        warning( paste(vars[!inRDA], collapse = ", "), " not found in RDA stream", file)
        vars = vars[inRDA]
    }
    
    new.vars = names(vars)
    if(length(new.vars) == 0) 
        names(vars) = new.vars = vars
    else if(any(w <- (names(vars) == "")))
         new.vars[w] = vars[w]        
    
    
    ok = mapply(doAssign, vars, new.vars, MoreArgs = list(from = e, to = envir, .noOverwrite = .noOverwrite))
    
    vars[is.na(ok) | ok]
}

doAssign =
function(var, newVar, from, to, .noOverwrite = TRUE)
{
    if(!.noOverwrite || !exists(newVar, to, inherits = FALSE)) {
        assign(newVar, get(var, e), to)
        TRUE
    } else
        FALSE
}

