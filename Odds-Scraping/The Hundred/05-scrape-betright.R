# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=5845"

# Make request and get response
betright_response <-
  request(betright_url) |>
  req_perform() |> 
  resp_body_json()

# Get matches
matches <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Keep only matches
matches <-
  matches |> 
  keep(~ .x$masterEventClassName == "Matches")

# Function to extract market info from response---------------------------------
get_market_info <- function(market) {
  
  # Market info
  markets_name = market$eventName
  market_propositions = market$outcomeName
  market_prices = market$price
  
  # Output Tibble
  tibble(market = markets_name,
         propositions = market_propositions,
         prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
  # Match info
  match_name = matches$masterEventName
  match_start_time = matches$minAdvertisedStartTime
  match_id = matches$masterEventId
  
  # Market info
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    match_id = match_id,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices
  )
}

# Map functions to data
all_betright_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Match Result")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Match Result")) |> 
  mutate(market_name = "Head To Head") |>
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = prices) |> 
  select(-propositions)

# Combine
betright_head_to_head_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "BetRight")

# Write to csv
write_csv(betright_head_to_head_markets, "Data/T20s/The Hundred/scraped_odds/betright_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Popular
popular_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G62&format=json")

# Top Run Scorer
top_run_scorer_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G107&format=json")

# Totals
totals_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G83&format=json")

# First Over
first_over_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G732&format=json")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
  
  # Get response
  response <-
    request(link) |>
    req_perform() |> 
    resp_body_json()
  
  # Empty vectors to append to
  event_name <- c()
  event_id <- c()
  outcome_title <- c()
  outcome_name <- c()
  outcome_id <- c()
  group_by_header <- c()
  fixed_market_id <- c()
  price <- c()
  
  for (event in response$events) {
    for (outcome in event$outcomes) {
      event_name <- c(event_name, event$eventName)
      event_id <- c(event_id, event$eventId)
      outcome_title <- c(outcome_title, outcome$eventName)
      outcome_name <- c(outcome_name, outcome$outcomeName)
      outcome_id <- c(outcome_id, outcome$outcomeId)
      group_by_header <- c(group_by_header, outcome$groupByHeader)
      fixed_market_id <- c(fixed_market_id, outcome$fixedMarketId)
      price <- c(price, outcome$price)
    }
  }
  
  # Output Tibble
  tibble(
    event_name = event_name,
    event_id = event_id,
    outcome_title = outcome_title,
    outcome_name = outcome_name,
    outcome_id = outcome_id,
    group_by_header = group_by_header,
    fixed_market_id = fixed_market_id,
    price = price,
    link
  )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

#-------------------------------------------------------------------------------
# Get popular markets data------------------------------------------------------
#-------------------------------------------------------------------------------

# Match names to join
match_names <-
  all_betright_markets |>
  distinct(match, match_id)

# Popular
betright_popular_markets <-
  map(popular_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Highest Opening Partnership
highest_opening_partnership <-
  betright_popular_markets |>
  filter(str_detect(outcome_title, "Highest Opening Partnership")) |>
  select(match, 
         market_name = event_name,
         team = outcome_name,
         price) |> 
  mutate(agency = "BetRight")

# Most Fours
most_fours <-
  betright_popular_markets |>
  filter(str_detect(outcome_title, "Most Fours")) |>
  select(match, 
         market_name = event_name,
         team = outcome_name,
         price) |> 
  mutate(agency = "BetRight")

# Most Sixes
most_sixes <-
  betright_popular_markets |>
  filter(str_detect(outcome_title, "Most Sixes")) |>
  select(match, 
         market_name = event_name,
         team = outcome_name,
         price) |> 
  mutate(agency = "BetRight")

#===============================================================================
# Write to CSV
#===============================================================================

betright_player_points |> write_csv("Data/scraped_odds/betright_player_points.csv")
