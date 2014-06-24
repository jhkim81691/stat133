median.norm = function(x) {
    # x: p by n data matrix
    #    where columns are the samples and rows are observations
	n = ncol(x)
    medians = apply(x, 2, median)
    reference = mean(medians)
    d = reference - medians
    norm = sapply(1:n,  function(i) x[,i]+d[i])
    dimnames(norm) = dimnames(x)
    return(norm)
}

### exercise 1
percentile.norm = function(x, prob=0.75) {
    # x: p by n data matrix
    #    where columns are the samples and rows are observations
    # 
    # Should work like median.norm when prob=0.5, but will
    # use quantile function rather than the median function.
    # By default it will align the distributions by
    # the third quartile. 

	n = ncol(x)
    medians = apply(x, 2, quantile, probs=prob)
    reference = mean(medians)
    d = reference - medians
    norm = sapply(1:n,  function(i) x[,i]+d[i])
    dimnames(norm) = dimnames(x)
    return(norm)
}

full.quantile.norm = function(x) {
    # x: p by n data matrix
    #    where columns are the samples and rows are observations

    x.sort = apply(x, 2, sort)    # sort within sample
    x.rank = apply(x, 2, rank)    # rank within sample
    reference = rowMeans(x.sort)
    norm = apply(x.rank, 2, function(smpl) reference[smpl])
    dimnames(norm) = dimnames(x)
    return(norm)
}

### exercise 2
full.quantile.norm2 = function(x) {
    # x: p by n data matrix
    #    where columns are the samples and rows are observations
    #
    # Should work like full.quantile.norm, but the reference will use the median
    # for each row.

    x.sort = apply(x, 2, sort)    # sort within sample
    x.rank = apply(x, 2, rank)    # rank within sample
    reference = rowMeans(x.sort, 1, median)
    norm = apply(x.rank, 2, function(smpl) reference[smpl])
    dimnames(norm) = dimnames(x)
    return(norm)
}


### exercise 3
full.quantile.norm3 = function(x) {
    # x: p by n data matrix
    #    where columns are the samples and rows are observations
    #
    # Should work like full.quantile.norm, but the reference will use the third
    # quartile for each row.

    x.sort = apply(x, 2, sort)    # sort within sample
    x.rank = apply(x, 2, rank)    # rank within sample
    reference = rowMeans(x.sort, 1, quantile, probs=0.75)
    norm = apply(x.rank, 2, function(smpl) reference[smpl])
    dimnames(norm) = dimnames(x)
    return(norm)
}
