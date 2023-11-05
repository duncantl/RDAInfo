library(methods)
setClass("A", representation(i = "integer"))
setClass("B", representation(d = "numeric"), contains = "A")

x = new("B", i = c(1L, 3L, 10L), d = c(101.2, 200.1))
save(x, file = "../inst/sampleRDA/S4_subclass.rda")
