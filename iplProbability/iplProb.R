Sys.time()
library(data.table)

baseTable <- read.csv("teams.csv", stringsAsFactors=FALSE)
teams <- baseTable$Team
basePts <- baseTable$Points
nrr <- baseTable$NRR

tally <- basePts + nrr 
names(tally) <- teams 

# Update the result after every match in schedule.csv
schedule <- read.csv("schedule.csv", stringsAsFactors=FALSE)
hosts <- schedule$Host
visitors <- schedule$Visitor
results <- Filter(function(x) x != "", schedule$Result)

remGames <- seq(hosts)[-seq(results)]
curHosts <- hosts[remGames]
curVisitors <- visitors[remGames]

# This is a closure: return value is a function
getWins <- function(y) { return(function(x){return(length(which(y==x)))}) }
# getPoints is for all teams whereas getWins is for a particular team
getPoints <- function(x) { return(2 * sapply(teams, getWins(x))) }

resultPoints <- getPoints(results)
curTally <- tally + resultPoints

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
    futurePoints <- getPoints(winTeams)
    finalPoints <- curTally + futurePoints
    top4 <- names(sort(finalPoints, decreasing=TRUE)[1:4])
    sapply(top4, addFn)
}

probTable <- data.table(Team = teams, Probability = 
    sapply(teams, function(x) eval(parse(text=paste(x)))/numRoutes))

Sys.time()
