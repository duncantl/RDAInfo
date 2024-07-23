library(RDAInfo)

mkCtr =
function()
{
    map = RDAInfo:::SEXPMap
    counts = structure(rep(0L, length(map)), names = names(map))
    list(update = function(val) {
        i = match(val, names(map))
        counts[i] <<- counts[i] + 1L
    },
    counts = function()
                 counts)
    
}

ctr = mkCtr()
e = substitute(fun(sexpType), list(fun = ctr$update))
trace(RDAInfo:::ReadItem, e, at = 3, print = FALSE)

# From TODO.md
rdas = list.files("inst/sampleRDA", pattern = "\\.rda", full = TRUE)
tmp = structure(lapply(rdas, function(x) try(toc(x))), names = rdas)
names(tmp) = gsub("\\.rda", "", basename(rdas))
err = sapply(tmp, inherits, 'try-error')

k = ctr$counts()
cat("\n\nHave NOT processed\n")
print(names(k)[k == 0])

m = RDAInfo:::SEXPMap
names(m)[m < 100]

cat("\n\nHave processed\n")
print(names(k)[k > 0])


#
cat("Rinternal types not processed yet\n")
print(intersect(names(m)[m < 100], names(k)[k == 0]))

#
# DOTSXP - this is an actual instance of arguments matched to ..., not the ... symbol, etc.
# DOTSXP leads to PROMSXP and UNBOUNDVALUE_SXP objects appearing in the RDA.
# See dots.rda and promise.rda

# should we see

# WEAKREFSXP   


# FUNSXP - No (?). This is used as a mode rather than an actual type.

# Shouldn't see
# NILSXP
# CHARSXP
# PROMSXP
# ANYSXP - used as a mode.
#
# S4SXP which is now OBJSXP
#

# NEWSXP or FREESXP - for allocation and GC.
