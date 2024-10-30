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

# manually change fips for alaska to match datawrapper fips for house districts
# change fips equal to 2901 to 2001, 2902 to 2002 and so on through 2940
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2901"] <- "2001"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2902"] <- "2002"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2903"] <- "2003"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2904"] <- "2004"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2905"] <- "2005"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2906"] <- "2006"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2907"] <- "2007"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2908"] <- "2008"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2909"] <- "2009"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2910"] <- "2010"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2911"] <- "2011"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2912"] <- "2012"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2913"] <- "2013"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2914"] <- "2014"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2915"] <- "2015"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2916"] <- "2016"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2917"] <- "2017"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2918"] <- "2018"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2919"] <- "2019"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2920"] <- "2020"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2921"] <- "2021"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2922"] <- "2022"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2923"] <- "2023"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2924"] <- "2024"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2925"] <- "2025"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2926"] <- "2026"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2927"] <- "2027"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2928"] <- "2028"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2929"] <- "2029"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2930"] <- "2030"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2931"] <- "2031"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2932"] <- "2032"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2933"] <- "2033"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2934"] <- "2034"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2935"] <- "2035"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2936"] <- "2036"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2937"] <- "2037"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2938"] <- "2038"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2939"] <- "2039"
all_county_candidates_pivoted$fips[all_county_candidates_pivoted$fips == "2940"] <- "2040"



# fix fips column to pad with leading zeros so it is always five characters in the string
all_county_candidates_pivoted$fips <- str_pad(all_county_candidates_pivoted$fips, width = 5, side = "left", pad = "0")



# Export the combined data frame as a CSV
write.csv(all_county_candidates_pivoted, "data/county_votes_all_states.csv", row.names = FALSE)