# Testing Benford's law using Sachin's scores in 463 ODI innings.
# https://en.wikipedia.org/wiki/Benford%27s_law

# I just happened to stumble upon the Wikipedia page of Benford's law (https://en.wikipedia.org/wiki/Benford%27s_law) today,  which states that 
# "In many naturally occurring collections of numbers, the leading significant digit is likely to be small" - To put in other words, when you gather 
# some collection of numbers, the numbers start with "1" more often than start with "2" and so on. 
# In the article, we see some examples such as the distribution of physical constants, the population of 237 countries which seem to obey the law. 
# Here we verify, taking a different data set (Of course, a proper verification would mean taking a larger number of such sets and finding the results)
# That exercise is for a different time, now taking just one set.  The data set is Sachin Tendulkar's ODI scores.  I took the data from cricinfo - 
# modified to have scores only from the matches where he scored (leaving out non-scoring or non-batting matches)
# And, here comes the distribution.

# firstDigit
#  1   2   3   4   5   6   7   8   9 
# 127  63  58  45  31  38  19  29  22

# And in Percentage terms,

# firstDigit
#    1     2     3     4     5     6     7     8     9 
# 29.40 14.58 13.43 10.42  7.18  8.80  4.40  6.71  5.09 
 
# which is of course, in line with Benford's law! 
# The occurrence of each digit is with a probability given by p(d) = log(1 + 1/d), log to the base 10
# I just wanted to see whether Benford's law is obeyed in other bases.
# Converted the scores into octal numbers and re-configured the probability formula to take in multiple bases - and the law applies.
# By the way, if you convert to binary number, there is only p(1) which is always 100%

require(XML)

benford <- function(d, b) {
    round( 100 * log(1 + (1/d), b), 2)
}

getDistribution <- function (charRuns){
    firstDigit <- sapply(charRuns, getFirstSigDigit)
    names(firstDigit) <- NULL 
    distribution <- table(firstDigit)
    return (distribution)
}

getDistPct <- function(distribution){
    distPct <- round(distribution / sum(distribution) * 100, 2)
    return(distPct)
}

getFirstSigDigit <- function(x){
    for (i in c(1:nchar(x))){
        curChar <- substr(x, i, i)
        if(curChar != '0'){
            return(curChar)
        }
    }
    return('')
}

theLink1 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;orderby=player;orderbyad=reverse;page=5;player_involve=1934;size=200;template=results;type=batting;view=innings"
theLink2 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;orderby=player;orderbyad=reverse;page=6;player_involve=1934;size=200;template=results;type=batting;view=innings"
theLink3 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=2;filter=advanced;orderby=player;orderbyad=reverse;page=7;player_involve=1934;size=200;template=results;type=batting;view=innings"

tab1 <- readHTMLTable(theLink1)
tab2 <- readHTMLTable(theLink2)
tab3 <- readHTMLTable(theLink3)
scores1 <- data.table(tab1['Innings by innings list'][[1]])
scores2 <- data.table(tab2['Innings by innings list'][[1]])
scores3 <- data.table(tab3['Innings by innings list'][[1]])
scores <- rbind(scores1, scores2, scores3)
sachinScores <- scores[Player == 'SR Tendulkar (INDIA)']

runs <- sachinScores[order(Runs)]$Runs
# Take only the matches where he batted and scored at least a run
battedRuns <- runs[which(!(runs == "0" | runs == "DNB" | runs == "TDNB"))]

charDecimalRuns <- gsub("\\*", "", as.character(battedRuns))
decimalRuns <- as.numeric(charDecimalRuns)

decimalDistribution <- getDistribution(charDecimalRuns)
decimalDistPct <- getDistPct(decimalDistribution)

octalRuns <- as.character(as.octmode(decimalRuns))
octalDistribution <- getDistribution(octalRuns)
octalDistPct <- getDistPct(octalDistribution)

hexRuns <- as.character(as.hexmode(decimalRuns))
hexDistribution <- getDistribution(hexRuns)
hexDistPct <- getDistPct(hexDistribution)

decimals <- c(1:9)
freqDecimals <- benford(decimals, 10)

octals <- c(1:7)
freqOctals <- benford(octals, 8)

hexs <- c(1:15)
freqHexs <- benford(hexs, 16)

barplot(freqDecimals, xlab="Starting Digit", ylab="Percentage", main="Benford Distribution", names.arg=decimals)
barplot(decimalDistPct, xlab="Starting Digit", ylab="Percentage", main="Sachin Scores", names.arg=decimals)

barplot(freqOctals, xlab="Starting Digit", ylab="Percentage", main="Benford (Octal)", names.arg=octals)
barplot(octalDistPct, xlab="Starting Digit", ylab="Percentage", main="Sachin Scores", names.arg=octals)

barplot(freqHexs, xlab="Starting Digit", ylab="Percentage", main="Benford (HexaDecimal)", names.arg=c(as.character(decimals),letters[1:6]))
barplot(hexDistPct, xlab="Starting Digit", ylab="Percentage", main="Sachin Scores", names.arg=c(as.character(decimals),letters[1:6]))
