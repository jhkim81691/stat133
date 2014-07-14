library(rbenchmark)

time.for = function(x) {
    out = rep(0, ncol(x))
    return(benchmark(for (i in 1:ncol(x)) out[i]=mean(x[, i]),
           columns = c('elapsed')))
}

time.apply = function(x) {
    return(benchmark(apply(x, 2, mean),
           columns = c('elapsed')))
}

time.func = function(x) {
    return(benchmark(colMeans(x),
           columns = c('elapsed')))
}

szs = seq(1000, 1000000, 50000)
x = lapply(szs, function(sz) matrix(rnorm(sz), 100))

for.times = sapply(x, time.for)
apply.times = sapply(x, time.apply)
func.times = sapply(x, time.func)

plot(szs, unlist(for.times), main="Benchmarks for computing column means",
     ylab="Time elapsed", type='l', col='blue')
lines(szs, unlist(apply.times), col='red')
lines(szs, unlist(func.times))
legend("topleft", c("For loop","Apply","C Function"),
      fill=c('blue','red','black'))
