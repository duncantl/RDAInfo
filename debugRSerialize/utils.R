# source("RDAXDR/R/SEXPMap.R")
library(RDAInfo)
library(RJSONIO)
readJS =
function(file = "/tmp/foo.json")
{
    df = as.data.frame(do.call(rbind, fromJSON(file)))
    names = c("type", "depth", "hastag", "hasattr")
    if(length(df) == 6)
        names = c(names, c("objf", "levs"))
    names(df) = names
    df[] = lapply(df[], as.integer)
    df[3:length(df)] = lapply(df[3:length(df)], as.logical)    
    df$typeName = sexpType(df$type)
    df
}


