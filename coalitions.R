library(tidyverse)
library(R.utils)
#library(plyr)

# poll Feb 16
election = tibble(
                  party = c("VVD","PVV","CDA","D66","GL","SP","PvdA","CU","PvdD","50PLUS","SGP","Denk","FvD", "JA21"),
                  seats = c( 38,    22,   20,  15,   11,  10,   12,   6,   7,       1,       2,     2,   3,     1),
     distancefromcenter = c(  -4,   -9,    -2,   2,   6,   8,    3,   -4,   7,      -2,     -8,     5,   -7,    5)
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

coalitions$partylist = sapply(numbers, function(i) 
                                            { pv = as.integer(unlist(strsplit(coalitions$bitcode[i], split="")))
                                              outv=""
                                              for(j in 1:length(election$party)) 
                                                  { 
                                                    if(pv[j]==1) 
                                                        outv = paste(outv,election$party[j])     
                                                  } 
                                              outv
                                            }
                              ) 

coalitions$numparties = sapply(numbers, function(i) 
                                            { pv = as.integer(unlist(strsplit(coalitions$bitcode[i], split="")))
                                              parties = sum(pv)
                                            }
                              ) 

coalitions$seatcount = sapply(numbers, function(i) 
                                            { pv = as.integer(unlist(strsplit(coalitions$bitcode[i], split="")))
                                              parties = sum(pv*election$seats)
                                            }
                              ) 

coalitions$compat = sapply(numbers, function(i) 
                                            { pv = as.integer(unlist(strsplit(coalitions$bitcode[i], split="")))
                                              pv2 = (pv*election$distancefromcenter)[pv*election$distancefromcenter!=0]
                                              parties = sd(pv2)
                                            }
                              ) 

coalitions$stability = -log((coalitions$numparties)^.6*(coalitions$compat)^.4)

majoritycoalitions <- coalitions %>% filter(seatcount>=75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(-stability, numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)

minoritycoalitions <- coalitions %>% filter(seatcount>=70) %>% 
                                     filter(seatcount<75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(-stability,numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)

minorityplus <- coalitions %>% filter(seatcount>=65) %>% 
                                     #filter(seatcount<75) %>% 
                                     filter(numparties <=6 ) %>% 
                                     arrange(-stability,numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)


View(majoritycoalitions)
View(minoritycoalitions)
View(minorityplus)

pv = as.integer(unlist(strsplit(coalitions$bitcode[8192], split="")))
p1 = sd((pv*election$distancefromcenter)[pv*election$distancefromcenter!=0])
p2 = p1[p1!=0]
parties = sd(p2)