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



# .Internal is a SPECIALSXP
save(.Internal, file = "inst/sampleRDA/dotInternal.rda")


# a DOTSXP
f = function(x, file = "inst/sampleRDA/dots.rda", ...) { dots = get("...");  save(dots, file = file); if(FALSE) plot(x, x^2, ...); TRUE}
f(1:10, xlab = "X", ylab = "Y")

# And empty ... raises an error for get("...")
#f(1:10, file = "inst/sampleRDA/dotsEmpty.rda")


f(1:10, "inst/sampleRDA/promise.rda", xlab = "X", ylab = cat("Saying hi\n"))



a = 1:3
n = NULL
save(a, n, file = "inst/sampleRDA/twoObjsOneNull.rda")
