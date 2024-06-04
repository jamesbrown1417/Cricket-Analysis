# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/Cricket/featured?homeState=SA&jurisdiction=SA"

# Get player metadata
player_meta_updated <- read_rds("Data/player_meta_updated.rds")

# Get player names and countries
player_names <-
  player_meta_updated |> 
  filter(dob >= "1980-01-01") |> 
  select(unique_name, full_name, country) |> 
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
  mutate(full_join_name = case_when(full_join_name == "Quinton Kock" ~ "Quinton de Kock",
                                    full_join_name == "Pasqual Mendis" ~ "Kamindu Mendis",
                                    full_join_name == "Pathum Silva" ~ "Pathum Nissanka",
                                    full_join_name == "Dhananjaya Silva" ~ "Dhananjaya De Silva",
                                    TRUE ~ full_join_name))

# Function to fetch and parse JSON with exponential backoff
fetch_data_with_backoff <-
  function(url,
           delay = 1,
           max_retries = 5,
           backoff_multiplier = 2) {
    tryCatch({
      # Attempt to fetch and parse the JSON
      tab_response <-
        read_html_live(url) |>
        html_nodes("pre") %>%
        html_text() %>%
        fromJSON(simplifyVector = FALSE)
      
      # Return the parsed response
      return(tab_response)
    }, error = function(e) {
      if (max_retries > 0) {
        # Log the retry attempt
        message(sprintf("Error encountered. Retrying in %s seconds...", delay))
        
        # Wait for the specified delay
        Sys.sleep(delay)
        
        # Recursively call the function with updated parameters
        return(
          fetch_data_with_backoff(
            url,
            delay * backoff_multiplier,
            max_retries - 1,
            backoff_multiplier
          )
        )
      } else {
        # Max retries reached, throw an error
        stop("Failed to fetch data after multiple retries.")
      }
    })
  }

tab_response <- fetch_data_with_backoff(tab_url)

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
  
  # Market info
  markets_name = markets$betOption
  market_propositions = markets$propositions
  
  # Output Tiiple
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
  market_info = map(matches$markets, get_market_info) |> bind_rows()
  
  # Output Tibble
  tibble(
    match = match_name,
    round = match_round,
    start_time = match_start_time,
    market_name = market_info$market,
    propositions = market_info$propositions
  )
}

# Get competitions
tab_competitions <-
  tab_response$competitions |> 
  map(~ .x$name)

# Get element of competitions that equals "ICC Twenty20 World Cup"
cwc_index <- which(tab_competitions == "ICC Twenty20 World Cup")

# Map functions to data
all_tab_markets <-
  map(tab_response$competitions[[cwc_index]]$matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest_wider(col = propositions, names_sep = "_") |>
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_name = propositions_name,
         price = propositions_returnWin)

#==============================================================================
# Head to head
#==============================================================================

# Filter to head to head markets
head_to_head <-
  all_tab_markets |>
  filter(market_name == "Head To Head") |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
  mutate(match = paste(home_team, "v", away_team, sep = " "))

# # Write to csv
write_csv(head_to_head, "Data/T20s/Internationals/scraped_odds/tab_h2h.csv")

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
  filter(str_detect(prop_name, "Over")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " Over ") |>
  mutate(line = str_remove(line, " Runs")) |>
  mutate(line = as.numeric(line)) |> 
  mutate(player_name = case_when(player_name == "G Erasmus" ~ "M Erasmus",
                                 player_name == "D De Silva" ~ "D de Silva",
                                 player_name == "S Ssesazi" ~ "S Sesazi",
                                 player_name == "S Smrwickrma" ~ "S Samarawickrama",
                                 player_name == "G Munsey" ~ "H Munsey",
                                 player_name == "M ODowd" ~ "M O'Dowd",
                                 .default = player_name)) |>
  left_join(player_names[, c("initials_join_name", "unique_name", "country")], by = c("player_name" = "initials_join_name")) |>
  select(-player_name) |> 
  rename(over_price = price,
         player_name = unique_name,
         player_team = country) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(home_team == player_team | away_team == player_team) |>
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |> 
  filter(player_name != "Aarif Sheikh")

# Get Unders
player_runs_unders <-
  player_runs_over_under |> 
  filter(str_detect(prop_name, "Under")) |>
  separate(prop_name, into = c("player_name", "line"), sep = " Under ") |>
  mutate(line = str_remove(line, " Runs")) |>
  mutate(line = as.numeric(line)) |> 
  mutate(player_name = case_when(player_name == "G Erasmus" ~ "M Erasmus",
                                 player_name == "D De Silva" ~ "D de Silva",
                                 player_name == "S Ssesazi" ~ "S Sesazi",
                                 player_name == "S Smrwickrma" ~ "S Samarawickrama",
                                 player_name == "G Munsey" ~ "H Munsey",
                                 player_name == "M ODowd" ~ "M O'Dowd",
                                 .default = player_name)) |>
  left_join(player_names[, c("initials_join_name", "unique_name", "country")], by = c("player_name" = "initials_join_name")) |>
  select(-player_name) |> 
  rename(under_price = price,
         player_name = unique_name,
         player_team = country) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(home_team == player_team | away_team == player_team) |>
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team))

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
    player_team,
    opposition_team,
    line,
    over_price,
    under_price
  ) |> 
  mutate(agency = "TAB")

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
  left_join(separated_names[, c("full_join_name", "unique_name", "country")], by = c("player_name" = "full_join_name")) |>
  select(-player_name) |> 
  rename(over_price = price,
         player_name = unique_name,
         player_team = country) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(home_team == player_team | away_team == player_team) |>
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |> 
  transmute(
    match,
    market = "Player Runs",
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price
  )

# Combine all player runs and write out-----------------------------------------
player_runs <-
  player_runs_over_under |>
  bind_rows(player_runs_alt) |>
  mutate(agency = "TAB") |> 
  arrange(match, player_name, line)

player_runs |>
  write_csv("Data/T20s/Internationals/scraped_odds/tab_player_runs.csv")

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
      player_name == "Matthew Kuhnemann" ~ "Matt Kuhnemann",
      .default = player_name
    )
  ) |>
  left_join(separated_names[, c("full_join_name", "unique_name", "country")], by = c("player_name" = "full_join_name")) |>
  select(-player_name) |> 
  rename(over_price = price,
         player_name = unique_name,
         player_team = country) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(home_team == player_team | away_team == player_team) |>
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |> 
  transmute(
    match,
    market = "Player Wickets",
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    agency = "TAB"
  )

# Combine all player wickets and write out-----------------------------------------
player_wickets_alt |> 
  write_csv("Data/T20s/Internationals/scraped_odds/tab_player_wickets.csv")

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
  left_join(separated_names[, c("full_join_name", "unique_name", "country")], by = c("player_name" = "full_join_name")) |>
  select(-player_name) |> 
  rename(over_price = price,
         player_name = unique_name,
         player_team = country) |> 
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(home_team == player_team | away_team == player_team) |>
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |> 
  mutate(market_name = if_else(str_detect(market_name, "Four"), "Number of 4s", "Number of 6s")) |>
  transmute(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    agency = "TAB"
  )

# Combine all player boundaries and write out-----------------------------------------
player_boundaries_alt |> 
  write_csv("Data/T20s/Internationals/scraped_odds/tab_player_boundaries.csv")

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

