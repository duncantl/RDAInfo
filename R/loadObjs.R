loadObjs =
function(file, ...)
{
    e = new.env()
    load(file, e)
    as.list.environment(e, TRUE, ...)
}
