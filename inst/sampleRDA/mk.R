e = emptyenv()
save(e, file = "inst/sampleRDA/emptyenv.rda")

s = as.name("abc")
save(s, file = "inst/sampleRDA/symbol.rda")


n = NULL
save(n, file = "inst/sampleRDA/NULL.rda")

rw0 = raw(10)
rw1 = readBin("inst/sampleRDA/mtcars.rda", raw(), n = 5)
save(rw0, rw1, file = "inst/sampleRDA/raw.rda")


f = function(x, ...) plot(x, rep(1, length(x)), ...)
save(f, file = "inst/sampleRDA/dots.rda")


ns = getNamespace("utils")
save(ns, file = "inst/sampleRDA/utilsNamespace.rda")

baseEnvNS = environment(ls)
save(baseEnvNS, file = "inst/sampleRDA/baseNamespace.rda")

save(.BaseNamespaceEnv, file = "inst/sampleRDA/baseNamespaceEnv.rda")

