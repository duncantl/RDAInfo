checkRDA =
    # load() the variables from an rda file and then
    # write each one to a separate file and read it back in via toc().
    # 
function(f)
{
    e = new.env()
    load(f, e)
    vars = ls(e, all = TRUE)
    structure(sapply(vars, checkToc, env = e), names = vars)
}

checkToc =
function(var, env, file = tempfile())
{
    save(list = var, envir = env, file = file)
    tryCatch( { toc(file); TRUE}, error = function(...) FALSE)
}

    
