# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(httr)
library(jsonlite)

# Get player metadata
player_meta_updated <- read_rds("Data/player_meta_updated.rds")

# Get player names and countries
player_names <-
  player_meta_updated |> 
  filter(dob >= "1980-01-01") |> 
  select(unique_name, full_name, country) |> 
  mutate(country = if_else(country == "U.S.A.", "USA", country)) |>
  # Split full name into first, middle and last names
  separate(unique_name, c("first_name", "last_name"), sep = " ", remove = FALSE, extra = "merge") |> 
  mutate(initials_join_name = paste(substr(first_name, 1, 1), last_name, sep = " ")) |> 
  mutate(initials_join_name = case_when(unique_name == "PHKD Mendis" ~ "K Mendis",
                                        TRUE ~ initials_join_name))

# Separate out middle names
separated_names <-
  player_names %>%
  mutate(
    first_name = sapply(strsplit(full_name, " "), `[`, 1),  # First element is the first name
    last_name = sapply(strsplit(full_name, " "), tail, 1), # Last element is the last name
    middle_names = sapply(strsplit(full_name, " "), function(x) {
      if (length(x) > 2) {
        paste(x[-c(1, length(x))], collapse=" ") # Collapse all but the first and last elements
      } else {
        NA  # No middle name
      }
    })
  ) |> 
  mutate(full_join_name = paste(first_name, last_name, sep = " ")) |> 
  distinct(full_join_name, .keep_all = TRUE) |>
  mutate(full_join_name = case_when(full_join_name == "Quinton Kock" ~ "Quinton de Kock",
                                    full_join_name == "Pasqual Mendis" ~ "Kamindu Mendis",
                                    full_join_name == "Pathum Silva" ~ "Pathum Nissanka",
                                    full_join_name == "Dhananjaya Silva" ~ "Dhananjaya De Silva",
                                    full_join_name == "Michael Lingen" ~ "Michael Van Lingen",
                                    full_join_name == "Shakib Hasan" ~ "Shakib Al Hasan",
                                    full_join_name == "Tanzid Tamim" ~ "Tanzid Hasan",
                                    full_join_name == "Najmul Shanto" ~ "Najmul Hossain Shanto",
                                    full_join_name == "Noor Lakanwal" ~ "Noor Ahmad",
                                    full_join_name == "Naveen-ul-Haq Murid" ~ "Naveen-ul-Haq",
                                    TRUE ~ full_join_name))

# Get response body
tab_response <- fromJSON("Odds-Scraping/BBL/TAB/tab_response.json")

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

# Function to fix team names for TAB BBL
fix_team_names <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Renegades") ~ "Melbourne Renegades",
    str_detect(team_name_vector, "Stars") ~ "Melbourne Stars",
    str_detect(team_name_vector, "Sixers") ~ "Sydney Sixers",
    str_detect(team_name_vector, "Thunder") ~ "Sydney Thunder",
    str_detect(team_name_vector, "Strikers") ~ "Adelaide Strikers",
    str_detect(team_name_vector, "Heat") ~ "Brisbane Heat",
    str_detect(team_name_vector, "Scorchers") ~ "Perth Scorchers",
    str_detect(team_name_vector, "Hurricanes") ~ "Hobart Hurricanes",
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
write_csv("Data/T20s/Big Bash/scraped_odds/tab_h2h.csv")

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
  mutate(player_name = case_when(player_name == "G Erasmus" ~ "M Erasmus",
                                 player_name == "D De Silva" ~ "D de Silva",
                                 player_name == "S Ssesazi" ~ "S Sesazi",
                                 player_name == "Will Jacks" ~ "William Jacks",
                                 player_name == "Phil Salt" ~ "Philip Salt",
                                 player_name == "Jos Buttler" ~ "Joseph Buttler",
                                 player_name == "George Munsey" ~ "Henry Munsey",
                                 player_name == "Max ODowd" ~ "Maxwell O'Dowd",
                                 player_name == "S Smrwickrma" ~ "Wedagedara Samarawickrama",
                                 player_name == "Jonny Bairstow" ~ "Jonathan Bairstow",
                                 player_name == "Ollie Hairs" ~ "Oliver Hairs",
                                 player_name == "Richie Berrington" ~ "Richard Berrington",
                                 .default = player_name)) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  transmute(
    match,
    market = "Player Runs",
    home_team,
    away_team,
    player_name,
    line,
    over_price = price
  ) |> 
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# TAB Names to join
tab_names <-
  player_runs_alt |> 
  select(unique_name = player_name) |> 
  distinct() |> 
  separate(unique_name, into = c("first_name", "last_name"), sep = " ", remove = FALSE, extra = "merge") |>
  mutate(initials_join_name = paste(substr(first_name, 1, 1), last_name, sep = " ")) |>
  select(-first_name, -last_name) |> 
  distinct(unique_name, initials_join_name)

#==============================================================================
# Player Runs Over / Under
#==============================================================================

# Filter to player runs over / under markets
player_runs_over_under <-
  all_tab_markets |>
  filter(market_name == "Player Runs" | market_name == "Alternate Player Runs O/U")

# Get Overs
player_runs_overs <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") |>
  mutate(line = str_remove(line, " Runs")) |>
  mutate(line = as.numeric(line)) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  rename(over_price = price)

# Get Unders
player_runs_unders <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " Under ") |>
  mutate(line = str_remove(line, " Runs")) |>
  mutate(line = as.numeric(line)) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  rename(under_price = price)

# Combine
player_runs_over_under <-
  player_runs_overs |>
  left_join(player_runs_unders) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    line,
    over_price,
    under_price
  ) |> 
  mutate(agency = "TAB") |> 
  mutate(player_name = case_when(player_name == "J F McGurk" ~ "J Fraser McGurk",
                                 TRUE ~ player_name)) |>
  left_join(tab_names, by = c("player_name" = "initials_join_name")) |>
  mutate(player_name = unique_name) |> 
  select(-unique_name) |> 
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |> 
  mutate(match = paste(home_team, "v", away_team, sep = " "))

# Combine all player runs and write out-----------------------------------------
player_runs <-
  player_runs_over_under |>
  bind_rows(player_runs_alt) |>
  mutate(agency = "TAB") |> 
  arrange(match, player_name, line)

player_runs |>
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_player_runs.csv")

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
  mutate(
    player_name = case_when(
      player_name == "Saurabh Nethralvakar" ~ "Saurabh Netravalkar",
      player_name == "Pat Cummins" ~ "Patrick Cummins",
      player_name == "Mohd. Siraj" ~ "Mohammed Siraj",
      player_name == "Tanzim Hasan Sakib" ~ "Tanzim Sakib",
      player_name == "Noshtush Kenjige" ~ "Nosthusha Kenjige",
      .default = player_name
    )
  ) |>
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
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Combine all player wickets and write out-----------------------------------------
player_wickets_alt |> 
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_player_wickets.csv")

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
  mutate(player_name = case_when(player_name == "G Erasmus" ~ "M Erasmus",
                                 player_name == "D De Silva" ~ "D de Silva",
                                 player_name == "S Ssesazi" ~ "S Sesazi",
                                 player_name == "Will Jacks" ~ "William Jacks",
                                 player_name == "Phil Salt" ~ "Philip Salt",
                                 player_name == "Jos Buttler" ~ "Joseph Buttler",
                                 player_name == "George Munsey" ~ "Henry Munsey",
                                 player_name == "Max ODowd" ~ "Maxwell O'Dowd",
                                 player_name == "S Smrwickrma" ~ "Wedagedara Samarawickrama",
                                 player_name == "Jonny Bairstow" ~ "Jonathan Bairstow",
                                 player_name == "Ollie Hairs" ~ "Oliver Hairs",
                                 player_name == "Richie Berrington" ~ "Richard Berrington",
                                 .default = player_name)) |>
  rename(over_price = price) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
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
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_player_boundaries.csv")

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
  mutate(team = case_when(team == "MStr" ~ "Melbourne Stars",
                          team == "Per" | team == "Pert" ~ "Perth Scorchers",
                          team == "SydS" ~ "Sydney Sixers",
                          team == "STh" ~ "Sydney Thunder",
                          team == "Hob" ~ "Hobart Hurricanes",
                          team == "MRn" | team == "MRng" ~ "Melbourne Renegades",
                          team == "Bri" ~ "Brisbane Heat",
                          team == "Ade" ~ "Adelaide Strikers",
                          TRUE ~ team)) |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team),
         away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_runs_at_first_wicket.csv")

#==============================================================================
# First Over Runs
#==============================================================================

# Filter to first over runs markets
first_over_runs <-
  all_tab_markets |>
  filter(market_name == "1st Over Runs")

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
  mutate(team = case_when(team == "MStr" ~ "Melbourne Stars",
                          team == "Per" | team == "Pert" ~ "Perth Scorchers",
                          team == "SydS" ~ "Sydney Sixers",
                          team == "STh" ~ "Sydney Thunder",
                          team == "Hob" ~ "Hobart Hurricanes",
                          team == "MRn" | team == "MRng" ~ "Melbourne Renegades",
                          team == "Bri" ~ "Brisbane Heat",
                          team == "Ade" ~ "Adelaide Strikers",
                          TRUE ~ team))  |>
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_first_over_runs.csv")

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
  mutate(team = case_when(team == "MStr" ~ "Melbourne Stars",
                          team == "Per" | team == "Pert" ~ "Perth Scorchers",
                          team == "SydS" ~ "Sydney Sixers",
                          team == "STh" ~ "Sydney Thunder",
                          team == "Hob" ~ "Hobart Hurricanes",
                          team == "MRn" | team == "MRng" ~ "Melbourne Renegades",
                          team == "Bri" ~ "Brisbane Heat",
                          team == "Ade" ~ "Adelaide Strikers",
                          TRUE ~ team))  |>
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_team_total_4s.csv")

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
  mutate(team = case_when(team == "MStr" ~ "Melbourne Stars",
                          team == "Per" | team == "Pert" ~ "Perth Scorchers",
                          team == "SydS" ~ "Sydney Sixers",
                          team == "STh" ~ "Sydney Thunder",
                          team == "Hob" ~ "Hobart Hurricanes",
                          team == "MRn" | team == "MRng" ~ "Melbourne Renegades",
                          team == "Bri" ~ "Brisbane Heat",
                          team == "Ade" ~ "Adelaide Strikers",
                          TRUE ~ team))  |>
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_team_total_6s.csv")

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
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_match_total_fours.csv")

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
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_match_total_sixes.csv")

#===============================================================================
# Most Team Wickets
#===============================================================================

# Filter to most team wickets markets
most_team_wickets <-
  all_tab_markets |>
  filter(market_name == "Most Wickets") |>
  mutate(team = str_extract(prop_name, "\\(.*\\)")) |>
  mutate(team = str_remove_all(team, "\\(|\\)")) |>
  mutate(team = case_when(team == "MStr" ~ "Melbourne Stars",
                          team == "Per" | team == "Pert" ~ "Perth Scorchers",
                          team == "SydS" ~ "Sydney Sixers",
                          team == "STh" ~ "Sydney Thunder",
                          team == "Hob" ~ "Hobart Hurricanes",
                          team == "MRn" | team == "MRng" ~ "Melbourne Renegades",
                          team == "Bri" ~ "Brisbane Heat",
                          team == "Ade" ~ "Adelaide Strikers",
                          TRUE ~ team)) |> 
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
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_top_team_wicket_taker.csv")

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
  write_csv("Data/T20s/Big Bash/scraped_odds/tab_highest_opening_partnership.csv")

