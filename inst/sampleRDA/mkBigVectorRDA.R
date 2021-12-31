N = 1e7
a = rnorm(N)
b = a+1
k = log(b)
k = exp(b)
save(a, b, k, file = "ThreeVectors_N1e7.rda", compress = FALSE)

str = sample(letters, N, replace = TRUE)
save(a, b, k, str, file = "ThreeVectors_CharVector_N1e7.rda", compress = FALSE)

fstr = factor(str)
save(a, b, k, fstr, file = "ThreeVectors_FactorVector_N1e7.rda", compress = FALSE)


