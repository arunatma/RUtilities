Sys.time()
library(data.table)

baseTable <- read.csv("teams.csv", stringsAsFactors=FALSE)

# Changing the names of the columns to the desired variable names.
# Name change done, column name change in Excel will not affect code
names(baseTable) <- c("teams", "basePts", "nrr")
list2env(baseTable, .GlobalEnv)     # creation of column variables in Env

tally <- basePts + nrr 
names(tally) <- teams 

# Update the result after every match in schedule.csv and nrr in teams.csv
# For no result, update the result as "NR" 
schedule <- read.csv("schedule.csv", stringsAsFactors=FALSE)
names(schedule) <- c("hosts", "visitors", "allResults") 
list2env(schedule, .GlobalEnv)      # creation of column variables in Env
results <- Filter(function(x) x != "", allResults)
noresults <- which(allResults == "NR")
nrTeams <- c(hosts[noresults], visitors[noresults])

remGames <- seq(hosts)[-seq(results)]
curHosts <- hosts[remGames]
curVisitors <- visitors[remGames]

# This is a closure: return value is a function
getWins <- function(y) { return(function(x){return(length(which(y==x)))}) }
# getPoints is for all teams whereas getWins is for a particular team
getPoints <- function(x, val) { return(val * sapply(teams, getWins(x))) }

resultPoints <- getPoints(results, 2)       # For each finished game
noresultPoints <- getPoints(nrTeams, 1)     # For each game with no result
curTally <- tally + resultPoints + noresultPoints
print(curTally)
numRemGames <- length(remGames)
numRoutes <- 2 ^ numRemGames
addFn <- function(x){return(eval(parse(text=paste0(x, "<<-", x, "+1"))))}

# creating variable names from a string vector
for(i in teams){
    assign(i, 0)    
}

for (i in (seq(numRoutes)-1)){
    homeWins <- rev(as.integer(intToBits(i))[1:numRemGames])
    winTeams <- ifelse(homeWins, curHosts, curVisitors)
    futurePoints <- getPoints(winTeams, 2)
    finalPoints <- curTally + futurePoints
    top4 <- names(sort(finalPoints, decreasing=TRUE)[1:4])
    sapply(top4, addFn)
}

probTable <- data.table(Team = teams, Probability = 
    sapply(teams, function(x) eval(parse(text=paste(x)))/numRoutes))

Sys.time()
