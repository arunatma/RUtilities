Sys.time()
library(data.table)

nrr <- c(0.471, 0.421, 0.145, 0.130, 0.070, -0.376, -0.411, -0.726)
teams <- c("SRH", "CSK", "KKR", "KXIP", "MI", "RCB", "DD", "RR")
basePts <- c(14, 14, 10, 10, 6, 6, 6, 6)

tally <- basePts + nrr 
names(tally) <- teams 

hosts <- c("MI", "KXIP", "SRH", "RR", "KKR", "DD", "RR", "KXIP", "DD", "CSK", 
    "MI", "KXIP", "KKR", "MI", "RCB", "DD", "RR", "SRH", "DD", "CSK")
visitors <- c("KKR", "RR", "RCB", "KXIP", "MI", "SRH", "CSK", "KKR", "RCB", "SRH",
    "RR", "RCB", "RR", "KXIP", "SRH", "CSK", "RCB", "KKR", "MI", "KXIP")

# Update the results here, after each match is completed    
results <- c("MI", "KXIP") #, "SRH", "RR", "KKR", "DD", "RR")

remMatches <- seq(hosts)[-seq(results)]
curHosts <- hosts[remMatches]
curVisitors <- visitors[remMatches]

# This is a closure: return value is a function
getWins <- function(y) { return(function(x){return(length(which(y==x)))}) }
# getPoints is for all teams whereas getWins is for a particular team
getPoints <- function(x) { return(2 * sapply(teams, getWins(x))) }

resultPoints <- getPoints(results)
curTally <- tally + resultPoints

numRemMatches <- length(remMatches)
numRoutes <- 2 ^ numRemMatches
addFn <- function(x){return(eval(parse(text=paste0(x, "<<-", x, "+1"))))}

# creating variable names from a string vector
for(i in teams){
    assign(i, 0)    
}

for (i in (seq(numRoutes)-1)){
    homeWins <- rev(as.integer(intToBits(i))[1:numRemMatches])
    winTeams <- ifelse(homeWins, curHosts, curVisitors)
    futurePoints <- getPoints(winTeams)
    finalPoints <- curTally + futurePoints
    top4 <- names(sort(finalPoints, decreasing=TRUE)[1:4])
    sapply(top4, addFn)
}

probTable <- data.table(Team = teams, Probability = 
    sapply(teams, function(x) eval(parse(text=paste(x)))/numRoutes))

Sys.time()
