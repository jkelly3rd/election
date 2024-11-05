library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)
library(stringr)
library(tidycensus)

# import live results for 2024
#all_counties24 <- read.csv("https://raw.githubusercontent.com/cbs-news-data/election-2024-maps/refs/heads/master/output/all_counties_clean_2024.csv")
# import the 2016 results from our data folder
all_counties16 <- read.csv("output/all_counties_clean_2016.csv")
all_counties20 <- read.csv("output/all_counties_clean_2020.csv")
all_counties24 <- read.csv("output/all_counties_clean_2024.csv")
winners16 <- all_counties16 %>%
  select(fips, leader, leader_margin_abs)
# name columns fips, winner, margin
names(winners16) <- c("fips", "winner_2016", "margin_2016")
# Repeat for 20
winners20 <- all_counties20 %>%
  select(fips, leader, leader_margin_abs)
names(winners20) <- c("fips", "winner_2020", "margin_2020")



# merge into a table that matches county by fips choosing total votes, trump votes and biden votes from 2020 and total votes, trump votes and harris votes from 2024
election_change <- all_counties20 %>% 
  select(fips, state, NAME, totalVote, vote_Donald.Trump, vote_Joe.Biden, pct_Donald.Trump,pct_Joe.Biden) %>% 
  left_join(all_counties24 %>% select(fips, totalVote, vote_Trump, vote_Harris, pct_Trump, pct_Harris), by = "fips") %>% 
  rename(total_vote_2020 = totalVote.x,
         total_vote_2024 = totalVote.y,
         vote_Trump_2020 = vote_Donald.Trump,
         vote_Biden_2020 = vote_Joe.Biden,
         vote_Trump_2024 = vote_Trump,
         vote_Harris_2024 = vote_Harris,
         pct_Trump_2020 = pct_Donald.Trump,
         pct_Biden_2020 = pct_Joe.Biden,
         pct_Trump_2024 = pct_Trump,
         pct_Harris_2024 = pct_Harris,
         county = NAME)

# merge in the 2016 winner
election_change <- election_change %>% 
  left_join(winners16, by = "fips") %>% 
  left_join(winners20, by = "fips")
# add column called 2020 flip if winner_2020 is different from winner_2016
election_change <- election_change %>%
  mutate(flip_status = case_when(
    winner_2020 == "Trump" & winner_2016 != "Trump" ~ "Flipped To Trump",
    winner_2020 == "Biden" & winner_2016 == "Trump" ~ "Flipped To Biden",
    TRUE ~ "Did Not Flip"
  ))

# Add a column for trump_vs_2000 that is pct_Trump_2024 > pct_Trump_2020
election_change <- election_change %>% 
  mutate(trump_vs_2020 = pct_Trump_2024 - pct_Trump_2020) %>%
  mutate(harris_vs_2020 = pct_Harris_2024 - pct_Biden_2020)
# Round those two resulting columns to nearest decimal
election_change <- election_change %>% 
  mutate(trump_vs_2020 = round(trump_vs_2020, 1)) %>%
  mutate(harris_vs_2020 = round(harris_vs_2020, 1))


# Create a trending_vs_2020 that is "Trump beating 2020" if pct_Trump_2024 > pct_Trump_2020 and "Harris beating 2020" if pct_Harris_2024 > pct_Biden_2020
election_change <- election_change %>% 
  mutate(trending_vs_2020 = case_when(
    pct_Trump_2024 > pct_Trump_2020 ~ "Trump beating 2020",
    pct_Harris_2024 > pct_Biden_2020 ~ "Harris beating 2020",
    TRUE ~ "No Trend Yet"
  ))

# Create a column called turnout_trend that is total_vote_2024 - total_vote_2020
election_change <- election_change %>% 
  mutate(turnout_trend = total_vote_2024 - total_vote_2020) %>%
  mutate(turnout_pct_up = turnout_trend / total_vote_2020 * 100)



