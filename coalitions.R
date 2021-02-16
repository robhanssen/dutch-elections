library(tidyverse)
library(R.utils)
#library(plyr)

# poll Feb 16
election = tibble(
                  party = c("VVD","PvdA","PVV","SP","CDA","D66","CU","GL","SGP","PvdD","50PLUS","VNL","Denk", "FvD","PP"),
                  seats = c( 33,    9,     20,  14,  19,   19,    5,  14,   3,    5,       4,     0,     3,     2,    0)
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
                    numparties = 0
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



majoritycoalitions <- coalitions %>% filter(seatcount>=75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)

minoritycoalitions <- coalitions %>% filter(seatcount>=70) %>% 
                                     filter(seatcount<=75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)


