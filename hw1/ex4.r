# Included in this folder, you will find the dataset "world_cup.data". This file
# contains data on the 2010 World Cup. When you load this data into your
# workspace, you should find that there are 32 observations (teams) with the
# following variables:
#
# <team>: the name of the team for each of the 32 participants
# <gp>: the number of games played over by a team over the entire tournament
# <gs> the number of goals scored by a team over the entire tournament
# <shots>: the number of shots taken over the course of the tournament
# <ga>: the number of goals scored against a team over the course of the
# tournament
# <fouls>: the number of fouls commited by a team over the course of the
# tournament
# <yc>: the total numer of yellow cards earned by a team over the course of the
# tournament
# <rc>: the number of red cards received by a team over the course of the
# tournament

# Implement the function "avgGoalDiff" which gives the average goal differential
# per game (i.e. (goals.for - goals.against)/games.played). Your function should
# take the following variables:
#
# <data>: a data frame representing any subset of the world cup data
# <team.name>: a character string giving (equal to one of the team names in
# <data>) giving the name of the team for which the average goal differential
# will be calculated
#
# Your function should return
#
# <avg.diff>: a numeric giving the average goal differential per game played by
# the given team.

avgGoalDiff <- function(data, team.name) {
	team.subset <- data[data$team == team.name, ]
	avg.diff <- (team.subset$gs-team.subset$ga)/team.subset$gp
	
	return (avg.diff)
}

# Implement the function "cardFoulRatio" which gives the ratio of cards (red or
# yellow) to fouls (i.e. (red.cards + yellow.cards) / fouls). Your function
# should take the following variables:
#
# <data>: a data frame representing any subset of the world cup data
# <team.name>: a character string giving (equal to one of the team names in
# <data>) giving the name of the team for which the average goal differential
# will be calculated
#
# Your function should return
#
# <cf.ratio> the ratio of total card to fouls by the given team

cardFoulRatio <- function(data, team.name) {
	team.subset <- data[data$team == team.name, ]
	cf.ratio <- (team.subset$rc + team.subset$yc)/team.subset$fouls
	
	return (cf.ratio) 
}

# Implement the function "rankAGD" which ranks teams (from highest to lowest)
# based on their average goa differential. HINT: you'll probably want to use the
# function you defined earlier. Your function should take the following
# argument:
#
# <data>: a data frame representing *any* subset of the world cup data
#
# Your function should return:
#
# <ranked.teams>: a vector of team names ordered (from highest to lowest) in
# terms of their average goal differential

rankAGD <- function(data) {
	data.avg.diff <- avgGoalDiff(data, data$team)
	sort.avg.diff <- sort(data.avg.diff, decreasing=T, index.return=T)
	ranked.teams <- data$team[sort.avg.diff[[2]]]
	
	return (ranked.teams)
}

# Load the data here. If you want to use the supplied unit tests, you must keep
# the name <wc.data>. Use your "cardFouldRation" function with sapply statement
# to find the ratio of cards to fould for each team. Store this variable as
# <cfr.teams>. Subset the <wc.data> dataset to include only teams with card foul
# ratios less than 0.12. Store this varialbe as <low.cfr.teams>. Run your
# "rankAGD" function on this subset and store the variable as <low.cfr.rank>.

wc.data <- read.table("world_cup.data", header=T)
cfr.teams <- sapply(as.character(wc.data$team), function(x,y) cardFoulRatio(y, x), y=wc.data)
low.cfr.teams <- wc.data[cfr.teams<0.12, ]
low.cfr.rank <- rankAGD(low.cfr.teams)


library(RUnit)
errMsg <- function(err) print(paste('ERROR:', err))

# test for avgGoalDiff
tryCatch(checkEquals(0.833333, avgGoalDiff(wc.data, 'Spain'), tolerance=1e-5),
         error=function(err) errMsg)

# test for cardFoulRatio
tryCatch(checkEquals(0.1153846, cardFoulRatio(wc.data, 'England'),
                     tolerance=1e-5), error=function(err) errMsg(err))

# test for rankAGD
tryCatch(checkIdentical(c('Argentina', 'Algeria', 'Australia'),
                        as.character(rankAGD(wc.data[1:3, ]))),
         error=function(err) errMsg(err))
