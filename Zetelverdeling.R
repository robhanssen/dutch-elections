#
# R program to determine the seats distribution in the Dutch Tweede Kamer after the 2017 election
# 

library(tidyverse)
library(readr)

# number of seats in the Tweede Kamer
totalseats = 150

# sample set of parties
parties <- c("VVD","PvdA","GL","D66","PVV","VNL","Denk")
votes <- c(12000,7000,10002,10020,11000,300,4000)
election <- data.frame(parties,votes)

rm(votes,parties)
#election <- read_csv("~/8 - JMP Demo/DutchElections/2012elections.csv")

# total number of valid votes casts
totalvotes = sum(election$votes)

# initial distribution of seats, 
election$seats <- floor( election$votes / totalvotes * totalseats )


#
# Elimination of parties that do not have seats out of the residual seats calculation
#
# Parties that did not win seats in the original calculation have no rights to residual seats
# according to law
#
election <-election[election$seats>0,]


# determination of how many seats out of the 150 have been distributed
givenseats = sum(election$seats)

# some temporary values to show how many residual seats parties pick up
election$extraseats = 0
election$totalseats = election$seats

#
# residual seat algorithm:
# 1. divide party votes by (current # of party seats +1)
# 2. the party with the highest number wins an additional seat
# 3. repeat with new number of seats per party until all residual seats are given away
#


while(givenseats < totalseats)
{
  election$residquot <- election$votes / (election$seats + election$extraseats + 1)
  maxresid <- max(election$residquot)
  election$extraseats[election$residquot==maxresid] <- election$extraseats[election$residquot==maxresid]+1
  election$totalseats = election$seats + election$extraseats
  givenseats <- sum(election$totalseats)
}

# removal of residquot values and ordering by number of seats
election <- election %>% arrange(-totalseats) %>% select(-residquot) 

# clean out
rm(maxresid)

# nice plot
ggplot(election, aes(x=reorder(parties,-totalseats),y=totalseats)) +
     geom_bar(stat="identity") + 
     xlab("Parties")+
     ylab("Total number of seats")
