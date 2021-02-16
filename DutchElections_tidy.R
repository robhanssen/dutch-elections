#
# R program to determine the seats distribution in the Dutch Tweede Kamer after the 2017 election
# 
library(tidyverse)
# library(ggplot2)
# library(dplyr)
# library(readr)
library(R.utils)
# library(plyr)

# number of seats in the Tweede Kamer
totalseats = 150

# election result
#election <- read_csv("C:/Users/rh0008/Google Drive/Documents/JMP/Dutch Elections/zetels.csv")
election <- read_csv("sources/2017votecount.csv")
# total number of valid votes casts
totalvotes = sum(election$votes)

kiesdeler = totalvotes/totalseats

# initial distribution of seats, 
# election$seats <-  floor(election$votes / totalvotes * totalseats)
#
# Elimination of parties that do not have seats out of the residual seats calculation
#
# Parties that did not win seats in the original calculation have no rights to residual seats
# according to law
#
election <- election %>% 
              mutate( seats = floor(votes / totalvotes * totalseats),
                      extraseats = 0, 
                      totalseats = seats) %>% 
              filter(seats != 0)

givenseats = sum(election$seats)

# some temporary values to show how many residual seats parties pick up
# election$extraseats = 0
# election$totalseats = election$seats

#
# residual seat algorithm:
# 1. divide party votes by (current # of party seats +1)
# 2. the party with the highest number wins an additional seat
# 3. repeat with new number of seats per party until all residual seats are given away
#


while(givenseats < totalseats)
{
  election <- election %>% mutate(residquot = votes / (seats + extraseats + 1))
  maxresid <- max(election$residquot)
  # election$extraseats[election$residquot==maxresid] <- election$extraseats[election$residquot==maxresid]+1
  election <- election %>% filter(residquot == maxresid) %>% mutate(extraseats = extraseats + 1) %>% full_join(election, by=c("parties", "votes", "seats","extraseats")) #%>% View()
  election$totalseats <- election$seats + election$extraseats
  givenseats <- sum(election$totalseats)
}

# removal of residquot values and ordering by number of seats
# election <- election %>% arrange(-totalseats) %>% select(-residquot) 

View(election)

# nice plot
ggplot(election, aes(x=reorder(parties,-totalseats),y=totalseats)) +
  geom_bar(stat="identity") + 
  xlab("Parties")+
  ylab("Total number of seats")


#election$party<-election$parties
#election <- data.frame(party,seats) %>% arrange(-seats) %>% filter(seats>0)

election$partycount = 1

partycount = sum(election$partycount)

election$partynum = 1:partycount

numbers = 1:(2^partycount-1)
bitcode = intToBin(numbers)
#bitcode = int2bin(numbers, lenght=16)

coalitions = data.frame(numbers,bitcode)
coalitions$partylist <- ""
coalitions$seatcount <- 0

partyvector<-laply(seq(1,partycount), function(i) as.integer(substr(coalitions$bitcode, i, i)))

for (i in numbers)
{
  coalitions$seatcount[numbers==i] <- sum(partyvector[,i] * election$totalseats)
  coalitions$numparties[numbers==i] <- sum(partyvector[,i])
  k = 0
  for (index in partyvector[,i]) 
  {
    k <- k + 1
    if(index==1) coalitions$partylist[numbers==i] <- paste(coalitions$partylist[numbers==i],election$parties[k]) 
  }
}

majoritycoalitions <- coalitions %>% filter(seatcount>=75) %>% 
  filter(numparties <=5 ) %>% 
  arrange(numparties,-seatcount) %>% 
  select(-numbers,-bitcode)

minoritycoalitions <- coalitions %>% filter(seatcount>=70) %>% 
  filter(seatcount<=75) %>% 
  filter(numparties <=5 ) %>% 
  arrange(numparties,-seatcount) %>% 
  select(-numbers,-bitcode)

# clean out
rm(maxresid,bitcode,i,k,index,numbers,partycount,totalvotes,givenseats,totalseats,partyvector)


