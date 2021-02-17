library(tidyverse)
library(R.utils)
#library(plyr)

# poll Feb 16
election = tibble(
                  party = c("VVD","PVV","CDA","D66","GL","SP","PvdA","CU","PvdD","50PLUS","SGP","Denk","FvD", "JA21"),
                  seats = c( 38,    22,   20,  15,   11,  10,   12,   6,   7,       1,       2,     2,   3,     1),
     distancefromcenter = c(  -3,   -9,    -2,   2,   4,   8,    3,   -5,   7,      -2,     -7,     5,   -7,    5)
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
                                              parties = sd(pv*election$distancefromcenter)
                                            }
                              ) 

coalitions$stability = -log((coalitions$numparties)^.4*(coalitions$compat)^.6)

majoritycoalitions <- coalitions %>% filter(seatcount>=75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(-stability, numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)

minoritycoalitions <- coalitions %>% filter(seatcount>=70) %>% 
                                     filter(seatcount<=75) %>% 
                                     filter(numparties <=5 ) %>% 
                                     arrange(numparties,-seatcount) %>% 
                                     select(-numbers,-bitcode)


View(majoritycoalitions)

majoritycoalitions %>% ggplot+aes(x=stability) + geom_density()
