# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(httr)
library(jsonlite)

# Get response body
tab_response <- fromJSON("Odds-Scraping/CPL/TAB/tab_response.json")

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_round = matches$round
    match_start_time = matches$startTime
    
    # Market info
    market_info =
        map(matches$markets, get_market_info) |>
        bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        round = match_round,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# List of matches
matches <- map(1:nrow(tab_response$matches), ~ tab_response$matches[., ])

# Map functions to data
all_tab_markets <-
  map(matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest(cols = c(propositions)) |> 
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_name = name,
         price = returnWin)

# Function to fix team names for TAB CPL
fix_team_names <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Antigua") ~ "Antigua and Barbuda Falcons",
    str_detect(team_name_vector, "St Kitts") ~ "St Kitts and Nevis Patriots",
    str_detect(team_name_vector, "Guyana") ~ "Guyana Amazon Warriors",
    str_detect(team_name_vector, "Trinbago") ~ "Trinbago Knight Riders",
    str_detect(team_name_vector, "Barbados") ~ "Barbados Royals",
    str_detect(team_name_vector, "St Lucia") ~ "St Lucia Kings",
    TRUE ~ team_name_vector
  )
}

#==============================================================================
# Head to head
#==============================================================================

# Filter to head to head markets
head_to_head <-
  all_tab_markets |>
  filter(market_name == "Head To Head") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " "))

# Home Win
home_win <-
  head_to_head |> 
  filter(prop_name == home_team) |> 
  select(
    match,
    market_name,
    home_team,
    away_team,
    home_win = price
  ) |> 
  mutate(agency = "TAB")
  
# Away Win
away_win <-
  head_to_head |> 
  filter(prop_name == away_team) |> 
  select(
    match,
    market_name,
    home_team,
    away_team,
    away_win = price
  ) |> 
  mutate(agency = "TAB")

# Join
h2h_new <-
  home_win |> 
  left_join(away_win) |> 
  mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
  relocate(agency, .after = away_win)

# Write to csv
h2h_new |> 
write_csv("Data/T20s/CPL/scraped_odds/tab_h2h.csv")

#==============================================================================
# Player Runs Alternate Lines
#==============================================================================

# Filter to player runs alt line markets
player_runs_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "^To Score")) |>
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  rename(over_price = price) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Player Runs",
    home_team,
    away_team,
    player_name,
    line,
    over_price
  )

#==============================================================================
# Player Runs Over / Under
#==============================================================================

# Filter to player runs over / under markets
player_runs_over_under <-
  all_tab_markets |>
  filter(market_name == "Player Runs")

# Get Overs
player_runs_overs <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Over|over")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " over ") |>
  mutate(line = str_remove(line, " runs")) |>
  mutate(line = as.numeric(line)) |> 
  rename(over_price = price) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Get Unders
player_runs_unders <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Under|under")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " under ") |>
  mutate(line = str_remove(line, " runs")) |>
  mutate(line = as.numeric(line)) |>
  rename(under_price = price) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)

# Combine
player_runs_over_under <-
  player_runs_overs |>
  left_join(player_runs_unders) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    line,
    over_price,
    under_price
  ) |> 
  mutate(agency = "TAB")

# Combine all player runs and write out-----------------------------------------
player_runs <-
  player_runs_over_under |>
  bind_rows(player_runs_alt) |>
  mutate(agency = "TAB") |> 
  arrange(match, player_name, line)

player_runs |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_player_runs.csv")

#==============================================================================
# Player Wickets Alternate Lines
#==============================================================================

# Filter to player wickets alt line markets
player_wickets_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "^To Take")) |>
  mutate(market_name = str_replace(market_name, " A ", " 1+ ")) |> 
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  rename(over_price = price) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  transmute(
    match,
    market = "Player Wickets",
    home_team,
    away_team,
    player_name,
    line,
    over_price,
    agency = "TAB"
  )

# Combine all player wickets and write out-----------------------------------------
player_wickets_alt |> 
  write_csv("Data/T20s/CPL/scraped_odds/tab_player_wickets.csv")

#==============================================================================
# Player Boundaries Alternate Lines
#==============================================================================

# Filter to player boundaries alt line markets
player_boundaries_alt <-
  all_tab_markets |>
  filter(str_detect(market_name, "To Hit")) |>
  filter(str_detect(market_name, "To Hit a Four and a Six", negate = TRUE)) |>
  mutate(market_name = str_replace(market_name, " A ", " 1+ ")) |> 
  mutate(line = str_extract(market_name, "\\d+")) |>
  mutate(line = as.numeric(line) - 0.5) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  rename(over_price = price) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(market_name = if_else(str_detect(market_name, "Four"), "Number of 4s", "Number of 6s")) |>
  transmute(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    line,
    over_price,
    agency = "TAB"
  )

# Combine all player boundaries and write out-----------------------------------------
player_boundaries_alt |> 
  write_csv("Data/T20s/CPL/scraped_odds/tab_player_boundaries.csv")

#==============================================================================
# Fall of first wicket
#==============================================================================

# Filter to fall of first wicket markets
fall_of_first_wicket <-
  all_tab_markets |>
  filter(market_name == "Fall Of 1st Wicket")

# Overs
fall_of_first_wicket_overs <-
  fall_of_first_wicket |>
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("team", "line"), sep = " Over ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(over_price = price)

# Unders
fall_of_first_wicket_unders <-
  fall_of_first_wicket |>
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("team", "line"), sep = " Under ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(under_price = price)

# Combine overs and unders and write out-----------------------------------------
fall_of_first_wicket_overs |>
  left_join(fall_of_first_wicket_unders) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Fall of 1st Wicket - Team",
    team,
    line,
    over_price,
    under_price,
    agency = "TAB"
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(-home_team, -away_team) |>
  mutate(team = case_when(team == "Ant" ~ "Antigua and Barbuda Falcons",
                          team == "StK" ~ "St Kitts and Nevis Patriots",
                          team == "Guy" ~ "Guyana Amazon Warriors",
                          team == "Tbg" ~ "Trinbago Knight Riders",
                          team == "Brb" | team == "Bar" ~ "Barbados Royals",
                          team == "StL" ~ "St Lucia Kings",
                          TRUE ~ team)) |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_runs_at_first_wicket.csv")

#==============================================================================
# First Over Runs
#==============================================================================

# Filter to first over runs markets
first_over_runs <-
  all_tab_markets |>
  filter(market_name == "First Over Runs O/U")

# Overs
first_over_runs_overs <-
  first_over_runs |>
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("team", "line"), sep = " Over ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(over_price = price)

# Unders
first_over_runs_unders <-
  first_over_runs |>
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("team", "line"), sep = " Under ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(under_price = price)

# Combine overs and unders and write out-----------------------------------------
first_over_runs_overs |>
  left_join(first_over_runs_unders) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "First Over Runs - Team",
    team,
    line,
    over_price,
    under_price,
    agency = "TAB"
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(-home_team, -away_team) |>
  mutate(team = case_when(team == "Ant" ~ "Antigua and Barbuda Falcons",
                          team == "StK" ~ "St Kitts and Nevis Patriots",
                          team == "Guy" ~ "Guyana Amazon Warriors",
                          team == "Tbg" ~ "Trinbago Knight Riders",
                          team == "Brb" | team == "Bar" ~ "Barbados Royals",
                          team == "StL" ~ "St Lucia Kings",
                          TRUE ~ team)) |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_first_over_runs.csv")

#===============================================================================
# Team Total 4s
#===============================================================================

# Filter to team boundaries markets
team_boundaries <-
  all_tab_markets |>
  filter(str_detect(market_name, "^Team Total Fours"))

# Overs
team_boundaries_overs <-
  team_boundaries |>
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("team", "line"), sep = " Over ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(over_price = price)

# Unders
team_boundaries_unders <-
  team_boundaries |>
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("team", "line"), sep = " Under ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(under_price = price)

# Combine overs and unders and write out-----------------------------------------
team_boundaries_overs |>
  left_join(team_boundaries_unders) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Team Total 4s",
    team,
    line,
    over_price,
    under_price,
    agency = "TAB"
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(-home_team, -away_team) |>
  mutate(team = case_when(team == "Ant" ~ "Antigua and Barbuda Falcons",
                          team == "StK" ~ "St Kitts and Nevis Patriots",
                          team == "Guy" ~ "Guyana Amazon Warriors",
                          team == "Tbg" ~ "Trinbago Knight Riders",
                          team == "Brb" | team == "Bar" ~ "Barbados Royals",
                          team == "StL" ~ "St Lucia Kings",
                          TRUE ~ team)) |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_team_total_4s.csv")

#===============================================================================
# Team Total 6s
#===============================================================================

# Filter to team boundaries markets
team_boundaries <-
  all_tab_markets |>
  filter(str_detect(market_name, "^Team Total Sixes"))

# Overs
team_boundaries_overs <-
  team_boundaries |>
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("team", "line"), sep = " Over ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(over_price = price)

# Unders
team_boundaries_unders <-
  team_boundaries |>
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("team", "line"), sep = " Under ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(under_price = price)

# Combine overs and unders and write out-----------------------------------------
team_boundaries_overs |>
  left_join(team_boundaries_unders) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Team Total 6s",
    team,
    line,
    over_price,
    under_price,
    agency = "TAB"
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(-home_team, -away_team) |>
  mutate(team = case_when(team == "Ant" ~ "Antigua and Barbuda Falcons",
                          team == "StK" ~ "St Kitts and Nevis Patriots",
                          team == "Guy" ~ "Guyana Amazon Warriors",
                          team == "Tbg" ~ "Trinbago Knight Riders",
                          team == "Brb" | team == "Bar" ~ "Barbados Royals",
                          team == "StL" ~ "St Lucia Kings",
                          TRUE ~ team)) |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_team_total_6s.csv")

#===============================================================================
# Match Total Fours
#===============================================================================

# Filter to match boundaries markets
match_boundaries <-
  all_tab_markets |>
  filter(str_detect(market_name, "^Total Fours Over/Under"))

# Overs
match_boundaries_overs <-
  match_boundaries |>
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("line"), sep = " Over ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(over_price = price)

# Unders
match_boundaries_unders <-
  match_boundaries |>
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("line"), sep = " Under ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(under_price = price)

# Combine overs and unders and write out-----------------------------------------
match_boundaries_overs |>
  left_join(match_boundaries_unders) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Match Total Fours",
    line,
    over_price,
    under_price,
    agency = "TAB"
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(-home_team, -away_team) |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_match_total_fours.csv")

#===============================================================================
# Match Total Sixes
#===============================================================================

# Filter to match boundaries markets
match_boundaries <-
  all_tab_markets |>
  filter(str_detect(market_name, "^Total Match Sixes Over/Under"))

alternate_sixes <-
  all_tab_markets |>
  filter(str_detect(market_name, "^Alternate Total Match Sixes"))

# Overs
match_boundaries_overs <-
  match_boundaries |>
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("line"), sep = " Over ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(over_price = price)

# Alt Overs
match_boundaries_alt_overs <-
  alternate_sixes |>
  mutate(line = as.numeric(str_extract(prop_name, "\\d+")) - 0.5) |>
  rename(over_price = price) |> 
  select(-market_name)

# Unders
match_boundaries_unders <-
  match_boundaries |>
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("line"), sep = " Under ") |>
  mutate(line = as.numeric(str_extract(line, "\\d+\\.\\d"))) |>
  rename(under_price = price)

# Combine overs and unders and write out-----------------------------------------
match_boundaries_overs |>
  bind_rows(match_boundaries_alt_overs) |>
  left_join(match_boundaries_unders) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Match Total Sixes",
    line,
    over_price,
    under_price,
    agency = "TAB"
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(-home_team, -away_team) |>
  write_csv("Data/T20s/CPL/scraped_odds/tab_match_total_sixes.csv")

#===============================================================================
# Most Team Wickets
#===============================================================================

# Filter to most team wickets markets
most_team_wickets <-
  all_tab_markets |>
  filter(market_name == "Most Wickets") |>
  mutate(team = str_extract(prop_name, "\\(.*\\)")) |>
  mutate(team = str_remove_all(team, "\\(|\\)")) |>
  mutate(
    team = case_when(
      team == "Ant" ~ "Antigua and Barbuda Falcons",
      team == "StK" ~ "St Kitts and Nevis Patriots",
      team == "Guy" ~ "Guyana Amazon Warriors",
      team == "Tbg" ~ "Trinbago Knight Riders",
      team == "Brb" | team == "Bar" ~ "Barbados Royals",
      team == "StL" ~ "St Lucia Kings",
      TRUE ~ team
    )
  ) |>
  mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(team = fix_team_names(team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  transmute(
    match,
    market = "Top Team Wicket Taker",
    home_team,
    away_team,
    player_name,
    player_team = team,
    opposition_team = if_else(player_team == home_team, away_team, home_team),
    price,
    agency = "TAB"
  )

# Write out
most_team_wickets |> 
  write_csv("Data/T20s/CPL/scraped_odds/tab_top_team_wicket_taker.csv")

#===============================================================================
# Highest Opening Partnership
#===============================================================================

# Filter to highest opening partnership markets
highest_opening_partnership <-
  all_tab_markets |>
  filter(market_name == "Highest Opening Partnership") |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(prop_name = fix_team_names(prop_name)) |>
  mutate(match = paste(home_team, "v", away_team))

# Get home team price
highest_opening_partnership_home <-
  highest_opening_partnership |>
  filter(home_team == prop_name) |> 
  transmute(match, home_team, away_team, market_name, home_price = price)

# Get tie price
highest_opening_partnership_tie <-
  highest_opening_partnership |>
  filter(prop_name == "tie") |> 
  transmute(match, home_team, away_team, market_name, tie_price = price)

# Get away team price
highest_opening_partnership_away <-
  highest_opening_partnership |>
  filter(away_team == prop_name) |> 
  transmute(match, home_team, away_team, market_name, away_price = price)

# Combine
highest_opening_partnership <-
  highest_opening_partnership_home |>
  left_join(highest_opening_partnership_tie) |>
  left_join(highest_opening_partnership_away) |>
  mutate(agency = "TAB")

# Write out
highest_opening_partnership |> 
  write_csv("Data/T20s/CPL/scraped_odds/tab_highest_opening_partnership.csv")

