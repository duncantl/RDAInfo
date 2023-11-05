source("RDAXDR/R/SEXPMap.R")
library(RJSONIO)
readJS =
function(file = "/tmp/foo.json")
{
    df =  structure(as.data.frame(do.call(rbind, fromJSON(file))), names = c("type", "depth", "hastag", "hasattr"))
    df[] = lapply(df[], as.integer)
    df[3:4] = lapply(df[3:4], as.logical)    
    df$typeName = sexpType(df$type)
    df
}


