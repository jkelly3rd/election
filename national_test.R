library(httr)
library(jsonlite)
library(tidyverse)
library(tidyr)

# Initialize an empty data frame to store the results
all_county_candidates_pivoted <- data.frame()

# Get a vector of state abbreviations
state_abbreviations <- c(state.abb)

# Loop through each state abbreviation
for (state in state_abbreviations) {
  # Construct the URL with the current state abbreviation
  json_url <- paste0("https://api-election.cbsnews.com/api/public/counties2/2020/G/", state, "/P")
  
  # GET the JSON data
  response <- GET(json_url)
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
    left_join(candidates %>% select(4, 6), by = "id")
  
  # Create a new table with one row per county, plus vote and pct results for each candidate
  county_candidates_pivoted <- county_candidates %>%
    # Group by the county-level columns
    group_by(name, fips, precinctsOf, precinctsIn, pctPrecIn, pctExpVote, totalVote, ts) %>%
    # Drop id
    select(-id) %>%
    # Spread the candidate information into separate columns
    pivot_wider(names_from = lastName, values_from = c(vote, pct)) %>%
    # Ungroup the data
    ungroup()
  
  # Add a column for difference between pct_trump and pct_biden
  county_candidates_pivoted <- county_candidates_pivoted %>%
    mutate(pct_diff = pct_Trump - pct_Biden)
  
  # Append the processed data to the results data frame
  all_county_candidates_pivoted <- bind_rows(all_county_candidates_pivoted, county_candidates_pivoted)
}

# fix fips column to pad with leading zeros so it is always five characters in the string
all_county_candidates_pivoted$fips <- str_pad(all_county_candidates_pivoted$fips, width = 5, side = "left", pad = "0")


# Export the combined data frame as a CSV
write.csv(all_county_candidates_pivoted, "data/county_votes_all_states.csv", row.names = FALSE)