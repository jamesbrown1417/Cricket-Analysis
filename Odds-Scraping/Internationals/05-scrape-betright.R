# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=72"

# Function to fix team names for BetRight Internationals
fix_team_names <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Antigua") ~ "Antigua and Barbuda Falcons",
    TRUE ~ team_name_vector
  )
}

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
  filter(str_detect(market_name, "Match Result|Match Winner")) |> 
  mutate(market_name = "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = prices) |> 
  select(-propositions)

# Away teams
away_teams <-
  all_betright_markets |>
  separate(match, into = c("away_team", "home_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Match Result|Match Winner")) |> 
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
write_csv(betright_head_to_head_markets, "Data/T20s/Internationals/scraped_odds/betright_h2h.csv")

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

# Player Runs
player_runs_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G313&format=json")

# Dismissals
dismissals_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G109&format=json")

# Totals
totals_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G83&format=json")

# First Over
first_over_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G732&format=json")

# First Six Overs
first_six_overs_links <-
  glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G735&format=json")

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
  
  if (is.null(event_name)) {
   event_name <- character() 
   event_id <- character()
   outcome_title <- character()
   outcome_name <- character()
   outcome_id <- character()
   group_by_header <- character()
   fixed_market_id <- character()
   price <- character()
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

#-------------------------------------------------------------------------------
# Get First Over Data-----------------------------------------------------------
#-------------------------------------------------------------------------------

# All First Over Markets
betright_first_over_markets <-
  map(first_over_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# First Over Team Runs----------------------------------------------------------
first_over_team_runs_overs <-
  betright_first_over_markets |>
  filter(str_detect(outcome_title, "First Over Total Runs")) |>
  filter(str_detect(outcome_name, "Over")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,3}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " First Over Total Runs .*$")) |>
  mutate(market = "First Over Runs - Team", agency = "BetRight") |>
  select(match, team = group_by_header, market, line, over_price = price, agency)

first_over_team_runs_unders <-
  betright_first_over_markets |>
  filter(str_detect(outcome_title, "First Over Total Runs")) |>
  filter(str_detect(outcome_name, "Under")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,3}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " First Over Total Runs .*$")) |>
  mutate(market = "First Over Runs - Team", agency = "BetRight") |>
  select(match, team = group_by_header, market, line, under_price = price, agency)

# Combine
first_over_runs <- 
  left_join(first_over_team_runs_overs, first_over_team_runs_unders) |> 
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(team = fix_team_names(team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, team, market, line, over_price, under_price, agency)

#-------------------------------------------------------------------------------
# Get Totals Data---------------------------------------------------------------
#-------------------------------------------------------------------------------

# All Totals Markets
betright_totals_markets <-
  map(totals_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Total Match Sixes-------------------------------------------------------------
total_match_sixes_overs <-
  betright_totals_markets |>
  filter(str_detect(outcome_title, "Total Match Sixes")) |>
  filter(str_detect(outcome_name, "Over")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,4}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " Total Match Sixes .*$")) |>
  mutate(market = "Total Match Sixes", agency = "BetRight") |>
  select(match, market, line, over_price = price, agency)

total_match_sixes_unders <-
  betright_totals_markets |>
  filter(str_detect(outcome_title, "Total Match Sixes")) |>
  filter(str_detect(outcome_name, "Under")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,4}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " Total Match Sixes .*$")) |>
  mutate(market = "Total Match Sixes", agency = "BetRight") |>
  select(match, market, line, under_price = price, agency)

# Combine
total_match_sixes <- 
  left_join(total_match_sixes_overs, total_match_sixes_unders) |> 
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, market, line, over_price, under_price, agency)

# Total Match Fours-------------------------------------------------------------
total_match_fours_overs <-
  betright_totals_markets |>
  filter(str_detect(outcome_title, "Total Match Fours")) |>
  filter(str_detect(outcome_name, "Over")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,4}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " Total Match Fours .*$")) |>
  mutate(market = "Total Match Fours", agency = "BetRight") |>
  select(match, market, line, over_price = price, agency)

total_match_fours_unders <-
  betright_totals_markets |>
  filter(str_detect(outcome_title, "Total Match Fours")) |>
  filter(str_detect(outcome_name, "Under")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,4}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " Total Match Fours .*$")) |>
  mutate(market = "Total Match Fours", agency = "BetRight") |>
  select(match, market, line, under_price = price, agency)

# Combine
total_match_fours <- 
  left_join(total_match_fours_overs, total_match_fours_unders) |> 
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, market, line, over_price, under_price, agency)

#-------------------------------------------------------------------------------
# Get Batsman Runs Data---------------------------------------------------------
#-------------------------------------------------------------------------------

# All Player Runs Markets
betright_player_runs_markets <-
  map(player_runs_links, safe_get_prop_data) |> 
  map("result") |>
  bind_rows() |> 
  rename(match_id = link) |> 
  mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
  left_join(match_names) |> 
  filter(!is.na(outcome_name))

# Batsman Runs------------------------------------------------------------------
batsman_runs_overs <-
  betright_player_runs_markets |>
  filter(str_detect(outcome_title, "Total Runs")) |>
  filter(str_detect(outcome_name, "Over")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,4}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " 1st Innings Total Runs .*$")) |>
  mutate(market = "Batsman Runs", agency = "BetRight") |>
  select(match, player_name = group_by_header, market, line, over_price = price, agency)

batsman_runs_unders <-
  betright_player_runs_markets |>
  filter(str_detect(outcome_title, "Total Runs")) |>
  filter(str_detect(outcome_name, "Under")) |>
  mutate(line = as.numeric(str_extract(outcome_name, "[0-9\\.]{1,4}"))) |>
  mutate(group_by_header = str_remove_all(group_by_header, " 1st Innings Total Runs .*$")) |>
  mutate(market = "Batsman Runs", agency = "BetRight") |>
  select(match, player_name = group_by_header, market, line, under_price = price, agency)

# Combine
batsman_runs <- 
  left_join(batsman_runs_overs, batsman_runs_unders) |> 
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, player_name, market, line, over_price, under_price, agency)

#===============================================================================
# Write to CSV
#===============================================================================

highest_opening_partnership |> write_csv("Data/T20s/Internationals/scraped_odds/betright_highest_opening_partnership.csv")
first_over_runs |> write_csv("Data/T20s/Internationals/scraped_odds/betright_first_over_runs.csv")
total_match_sixes |> write_csv("Data/T20s/Internationals/scraped_odds/betright_total_match_sixes.csv")
total_match_fours |> write_csv("Data/T20s/Internationals/scraped_odds/betright_total_match_fours.csv")
batsman_runs |> write_csv("Data/T20s/Internationals/scraped_odds/betright_player_runs.csv")

