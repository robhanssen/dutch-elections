library(tidyverse)
library(R.utils)

election = read_csv("sources/latestpoll2021.csv", comment="#") %>%
              filter(seats > 0) %>%
              arrange(-seats)

numbers = 1:(2^nrow(election)-1)
bitcode = intToBin(numbers)

coalitions = tibble(
                    numbers,
                    bitcode, 
                    partylist = "", 
                    seatcount = 0,
                    numparties = 0,
                    compat = 0, 
                    stability = 0
                    ) 

partycompatibility <- function(pvec, elec)
{
    numseatbyparty = pvec*election$seats
    repvec = rep(elec$distancefromcenter, numseatbyparty)
    sd(repvec)
}

resultlist = sapply(numbers, function(i) 
                                            { pv = as.integer(unlist(strsplit(coalitions$bitcode[i], split="")))
                                              # generate the party list by matching "1"s to names
                                              partylist = paste(rep(election$party,pv), collapse=" ")
                                              # count number of "1"s
                                              numparties = sum(pv)
                                              # count number of seats in total set
                                              seatcount = sum(pv*election$seats)
                                              # esotheric compatibility function
                                              compat = partycompatibility(pv, election)
                                              # output list
                                              list(partylist, numparties, seatcount, compat)
                                            }
                              ) 

coalitions$partylist = unlist(resultlist[1,])
coalitions$numparties = unlist(resultlist[2,])
coalitions$seatcount = unlist(resultlist[3,])
coalitions$compat = unlist(resultlist[4,])
coalitions$stability = 1/((coalitions$numparties)^.5*(coalitions$compat)^.5)

majoritycoalitions <- coalitions %>% filter(seatcount>= 75) %>% 
                                     filter(numparties <= 5 ) %>% 
                                     arrange(-stability, -seatcount, numparties) %>% 
                                     select(-numbers,-bitcode)

minoritycoalitions <- coalitions %>% filter(seatcount>=70) %>% 
                                     filter(seatcount<75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(-stability,numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)

minorityplus <- coalitions %>% filter(seatcount>=65) %>% 
                                     filter(numparties <=6 ) %>% 
                                     arrange(-stability,numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)

View(majoritycoalitions)
View(minoritycoalitions)
View(minorityplus)