library(httr)
library(jsonlite)
library(tidyverse)
library(tidyr)

# URL to the JSON data
json_url <- "https://api-election.cbsnews.com/api/public/counties2/2020/G/PA/P"

# GET the JSON data
response <- GET(json_url)

# get a vector of state abbreviations
state_abbreviations <- c(state.abb, "DC")

json_data <- content(response, "text")
data <- fromJSON(json_data, flatten = TRUE)

# Extract counties and candidates data
counties <- data$race$counties
candidates <- data$race$candidates

# Unnest the candidates within each county
county_candidates <- counties %>%
  unnest(candidates)

# Add lastNames to the candidates data
county_candidates <- county_candidates %>%
  left_join(candidates %>% select(4,6), by = "id")

# Create a new table with one row per county, plus vote and pct results for each candidate
  county_candidates_pivoted <- county_candidates %>%
# Group by the county-level columns
  group_by(name, fips, precinctsOf, precinctsIn, pctPrecIn, pctExpVote, totalVote, ts) %>%
# drop id
  select(-id) %>%
# Spread the candidate information into separate columns
  pivot_wider(names_from = lastName, values_from = c(vote, pct)) %>%
# Ungroup the data
  ungroup()

# Add a column for difference between pct_trump and pct_biden
county_candidates_pivoted <- county_candidates_pivoted %>%
  mutate(pct_diff = pct_Trump - pct_Biden)


# Export as a csv
write.csv(county_candidates_pivoted, "data/county_votes_pa.csv", row.names = FALSE)


  

  
  
  

