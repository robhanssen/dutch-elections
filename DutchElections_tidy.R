#
# R program to determine the seats distribution in the Dutch Tweede Kamer after the 2017 election
# 
library(tidyverse)
# library(R.utils)
theme_set(theme_light())

# number of seats in the Tweede Kamer
totalseats = 150

# election result
election <- read_csv("sources/2017votecount.csv")
# total number of valid votes casts
totalvotes <- sum(election$votes)

kiesdeler <- totalvotes / totalseats

# initial distribution of seats, 
#
# Elimination of parties that do not have seats out of the residual seats calculation
#
# Parties that did not win seats in the original calculation have no rights to residual seats
# according to law
#
election <- election %>%
              mutate(seats = floor(votes / totalvotes * totalseats),
                     extraseats = 0,
                     totalseats = seats) %>%
              filter(seats != 0)

#
# residual seat algorithm:
# 1. divide party votes by (current # of party seats +1)
# 2. the party with the highest number wins an additional seat
# 3. repeat with new number of seats per party until all residual seats are given away
#

raw_election <- election
election <- raw_election
givenseats <- sum(election$seats)

while (givenseats < totalseats)
{
  election <- election %>% mutate(residquot = votes / (seats + extraseats + 1))
  maxresid <- max(election$residquot)

  election <- election %>%
    mutate(extraseats = case_when(residquot == maxresid ~ extraseats + 1,
                                  TRUE ~ extraseats)
    )

  election$totalseats <- election$seats + election$extraseats
  givenseats <- sum(election$totalseats)
}

election

# removal of residquot values and ordering by number of seats
# election <- election %>% arrange(-totalseats) %>% select(-residquot) 


# nice plot
election %>%
  ggplot +
  aes(x = fct_reorder(parties, totalseats),
      y = totalseats) +
  geom_col(alpha = .8) +
  labs(x = "Party",
       y = "Number of seats") +
  coord_flip()

partycount <- election %>% count() %>% pull(n)

election$partynum <- 1:partycount

numbers <- 1:(2^partycount - 1)
bitcode <- R.utils::intToBin(numbers)

coalitions <- tibble(numbers, bitcode)
coalitions$partylist <- ""
coalitions$seatcount <- 0

decomp_vector <- function(vector) {
    v <- sort(strsplit(vector, "")[[1]])
    as.numeric(v)
}



# partyvector <-
#   lapply(seq_len(length(numbers)),
#          function(x) decomp_vector(coalitions$bitcode[x]))


coal <- coalitions %>%
    mutate(bitlist = lapply(seq_len(length(numbers)),
                            function(x) decomp_vector(coalitions$bitcode[x])),
           partycount = sum(unlist(bitlist)),
           seatcount = sum(unlist(bitlist) * election$totalseats)
    )


# coalitions$bitter <- partyvector #decomp_vector(coalitions$bitcode)


unlist(coal$bitlist[3]) * election$totalseats
sum(unlist(coal$bitlist[3])) #* election$totalseats


for (i in numbers)
{
  coalitions$seatcount[i] <- sum(partyvector[[i]] * election$totalseats)
  coalitions$numparties[i] <- sum(partyvector[[i]])
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
