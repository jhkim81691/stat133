# d = data.frame(x=c(anscombe$x1,anscombe$x2,anscombe$x3,anscombe$x4),
#                 y=c(anscombe$y1,anscombe$y2,anscombe$y3,anscombe$y4),
#                 z=gl(4,nrow(anscombe))
d = with(anscombe, data.frame(x=c(x1,x2,x3,x4),
                              y=c(y1,y2,y3,y4),
                              z=gl(4,nrow(anscombe))))


aggregate(.~z, d, mean)
aggregate(.~z, d, median)
aggregate(.~z, d, sd)
sapply(split(d, d$z), function(df) cor(df$x, df$y))
aggregate(x~z, d, summary)
aggregate(y~z, d, summary)

fit = by(d[,1:2], d$z, function(df) lm(y~x, df))
X = by(d[,1:2], d$z, identity)

par(mfrow=c(2,2), mar=0.1+c(4,4,1,1), oma=c(0,0,2,0))
mapply(function(x, y) {
         plot(x, xlim=c(3,20), ylim=c(3,13), col='red', pch=21, bg='orange')
         abline(y, col='blue')
       }, X, fit)
mtext("Anscombe's 4 Regression data sets", outer=TRUE, cex=1.5)
