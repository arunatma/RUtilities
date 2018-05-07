Sys.time()
library(data.table)
nrr <- c(0.471, 0.421, 0.145, 0.130, 0.070, -0.376, -0.411, -0.726)
teams <- c("SRH", "CSK", "KKR", "KXIP", "MI", "RCB", "DD", "RR")
tally <- c(14, 14, 10, 10, 6, 6, 6, 6)
tally <- tally + nrr 

names(tally) <- teams 

homes <- c("MI", "KXIP", "SRH", "RR", "KKR", "DD", "RR", "KXIP", "DD", "CSK", 
    "MI", "KXIP", "KKR", "MI", "RCB", "DD", "RR", "SRH", "DD", "CSK")
aways <- c("KKR", "RR", "RCB", "KXIP", "MI", "SRH", "CSK", "KKR", "RCB", "SRH",
    "RR", "RCB", "RR", "KXIP", "SRH", "CSK", "RCB", "KKR", "MI", "KXIP")
    
totalMatches <- length(homes)
    
results <- c("MI", "KXIP")
numResults <- length(results)

remMatches <- c(numResults + 1 : totalMatches)
curHomes <- homes[remMatches]
curAways <- aways[remMatches]

getWins <- function(x){return(length(which(results==x)))}

incrScores <- 2 * sapply(teams, getWins)      # 2 points for a win  
newScores <- tally + incrScores

numRemMatches <- length(remMatches)
numRoutes <- 2 ^ numRemMatches
addFn <- function(x){return(eval(parse(text=paste0(x, "<<-", x, "+1"))))}

# creating variable names from a string vector
for(i in teams){
    assign(i, 0)    
}

for (i in c(0:numRoutes-1)){
    homeWins <- rev(as.integer(intToBits(i))[1:numRemMatches])
    winTeams <- ifelse(homeWins, curHomes, curAways)
    scoreFn <- function(x){return(length(which(winTeams==x)))}
    fwdScores <- 2 * sapply(teams, scoreFn)
    roundScores <- newScores + fwdScores
    top4 <- names(sort(roundScores, decreasing=TRUE)[1:4])
    sapply(top4, addFn)
}

probTable <- data.table(Team = teams, Probability = 
    sapply(teams, function(x) eval(parse(text=paste(x)))/numRoutes))

Sys.time()
