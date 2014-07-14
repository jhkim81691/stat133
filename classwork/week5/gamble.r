
gamble <- function(init.wealth, prob.win, max.turns) {

    current.wealth <- init.wealth
    bet <- 0.05 * current.wealth
    games <- rbinom(max.turns, 1, prob.win)
    
    for (turn in 1:max.turns) {

        if (games[turn]) current.wealth <- current.wealth + bet
        else current.wealth <- current.wealth - bet

        if (current.wealth < 0) break
        if (current.wealth > 2.5 * init.wealth) break
    }

    return(current.wealth - init.wealth)
}


a <- Sys.time()
temp <- replicate(10000, gamble(500, .49, 500))
b <- Sys.time()
