library(tidyverse)
library(R.utils)

# poll Feb 16
election = tibble(
                  party = c("VVD","PVV","CDA","D66","GL","SP","PvdA","CU","PvdD","50PLUS","SGP","Denk","FvD", "JA21"),
                  seats = c( 38,    22,   20,  15,   11,  10,   12,   6,   7,       1,       2,     2,   3,     1),
                  # ranked mainly on right-left balance in Dutch politics; PVV and FvD are arbitrarily off because of cordon sanitaire
     distancefromcenter = c(  -4,   -15,  -2,   2,   6,   8,    3,   -4,   7,      -2,      -8,     5, -15,     5)
                  ) %>%
            filter(seats > 0) %>%
            arrange(-seats) %>%
            mutate(partynum =row_number())

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
                                              partylist = ""
                                              for(j in 1:length(election$party)) 
                                                  { 
                                                    if(pv[j]==1) 
                                                        partylist = paste(partylist, election$party[j])     
                                                  } 
                                              # count number of "1"s
                                              numparties = sum(pv)
                                              # count number of seats in total set
                                              seatcount = sum(pv*election$seats)
                                              # esotheric compatibility function
                                              #compat = sd((pv*election$distancefromcenter)[pv*election$distancefromcenter!=0])
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