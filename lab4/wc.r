# Suppose that goals in a world cup soccer game can be described by a
# Poisson process (though they may not be entirely independent). How
# unlikely was the Germany v Brazil game? We'll look at this from the angle
# of number of goals scored in particular intervals of time, disregarding
# which team scored

average.goals <- 2.74  # average goals/game in 2014
goal.times <- c(11, 23, 24, 26, 29, 69, 79, 90) # minute of goals for BvG
n <- 100000


# How unlikely was it for 8 goals total to be scored?
g.sims <- rpois(n, average.goals)
p.8g <- sum(g.sims > 7) / n


# Suppose we were interested in 15 minute intervals. How unlikely was the
# stretch from 15'-30' when Germany scored 4 goals? Disregarding extra
# time, we'll break the game into 6 intervals.
rate <- average.goals / 6
p.sims <- rpois(n, rate)
p.4g <- sum(p.sims > 3) / n


# Examine our simulations visually along with where GvB falls in the
# distribution 
plotPois <- function(sims, rate, observed) {

    x.grid <- min(sims):(max(sims)+1)
    hist(sims+0.01, freq=F, xlab='goals', breaks=x.grid, main='Simulations')
    lines(x.grid, dpois(x.grid, rate), col='red')
    rug(observed, ticksize=0.05, lwd=2, col='blue')
}

par(mfrow=c(2, 1))
plotPois(g.sims, average.goals, 8)

goals.period <- c(1, 4, 0, 0, 1, 2) #number of goals in each 15min period
plotPois(p.sims, rate, goals.period)

                                        


