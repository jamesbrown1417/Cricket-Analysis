# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Fix team names function


# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Odds-Scraping/Internationals/Bet365/HTML", full.names = TRUE, pattern = "player")

# Main Function
get_player_runs <- function(scraped_file) {
  # Get Markets
  bet365_player_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_player_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text ") |>
    html_text()
  
  #=============================================================================
  # Batter Match Runs
  #=============================================================================
  
  # Get index for node with text "Batter Match Runs"
  batter_match_runs_index <- which(market_names == "Batter Match Runs")
  
  # Get Player Names from node
  batter_match_runs_players <-
    bet365_player_markets[[batter_match_runs_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Get Player Teams from node
  batter_match_runs_teams <-
    bet365_player_markets[[batter_match_runs_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    html_text()
  
  # Get Over Node Index
  batter_match_runs_cols <-
    bet365_player_markets[[batter_match_runs_index]] |>
    html_elements(".gl-Market_General")
  
  batter_match_runs_over_index <- which(str_detect(batter_match_runs_cols |> html_text(), "Over"))
  
  # Get Over Lines
  batter_match_runs_over_lines <-
    batter_match_runs_cols[[batter_match_runs_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  batter_match_runs_over_odds <-
    batter_match_runs_cols[[batter_match_runs_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  batter_match_runs_under_index <- which(str_detect(batter_match_runs_cols |> html_text(), "Under"))
  
  # Get Under Odds
  batter_match_runs_under_odds <-
    batter_match_runs_cols[[batter_match_runs_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Batter Match Runs Table
  batter_match_runs <-
    tibble(player = batter_match_runs_players,
           team = batter_match_runs_teams,
           line = as.numeric(batter_match_runs_over_lines),
           over_price = as.numeric(batter_match_runs_over_odds),
           under_price = as.numeric(batter_match_runs_under_odds)) |>
    mutate(market_name = "Batter Match Runs") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Batter Milestones
  #=============================================================================
  
  # Get index for node with text "Batter Milestones"
  batter_milestones_index <- which(market_names == "Batter Milestones")
  
  # Get Player Names from node
  batter_milestones_players <-
    bet365_player_markets[[batter_milestones_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Get Player Teams from node
  batter_milestones_teams <-
    bet365_player_markets[[batter_milestones_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    html_text()
  
  # Get 10+ Runs Node Index
  batter_milestones_cols <-
    bet365_player_markets[[batter_milestones_index]] |>
    html_elements(".gl-Market_General")
  
  batter_milestones_10_index <- which(str_detect(batter_milestones_cols |> html_text(), "10\\+"))
  batter_milestones_20_index <- which(str_detect(batter_milestones_cols |> html_text(), "20\\+"))
  batter_milestones_30_index <- which(str_detect(batter_milestones_cols |> html_text(), "30\\+"))
  batter_milestones_50_index <- which(str_detect(batter_milestones_cols |> html_text(), "50\\+"))
  batter_milestones_70_index <- which(str_detect(batter_milestones_cols |> html_text(), "70\\+"))
  
  # Get 10+ Runs Odds
  batter_milestones_10_odds <-
    batter_milestones_cols[[batter_milestones_10_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 20+ Runs Odds
  batter_milestones_20_odds <-
    batter_milestones_cols[[batter_milestones_20_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 30+ Runs Odds
  batter_milestones_30_odds <-
    batter_milestones_cols[[batter_milestones_30_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 50+ Runs Odds
  batter_milestones_50_odds <-
    batter_milestones_cols[[batter_milestones_50_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 70+ Runs Odds
  batter_milestones_70_odds <-
    batter_milestones_cols[[batter_milestones_70_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Create Batter Milestones Tables
  batter_milestones_10 <-
    tibble(player = batter_milestones_players,
           team = batter_milestones_teams,
           line = 9.5,
           over_price = as.numeric(batter_milestones_10_odds)) |>
    mutate(market_name = "Batter Milestones") |>
    mutate(agency = "Bet365")
  
  batter_milestones_20 <-
    tibble(player = batter_milestones_players,
           team = batter_milestones_teams,
           line = 19.5,
           over_price = as.numeric(batter_milestones_20_odds)) |>
    mutate(market_name = "Batter Milestones") |>
    mutate(agency = "Bet365")
  
  batter_milestones_30 <-
    tibble(player = batter_milestones_players,
           team = batter_milestones_teams,
           line = 29.5,
           over_price = as.numeric(batter_milestones_30_odds)) |>
    mutate(market_name = "Batter Milestones") |>
    mutate(agency = "Bet365")
  
  batter_milestones_50 <-
    tibble(player = batter_milestones_players,
           team = batter_milestones_teams,
           line = 49.5,
           over_price = as.numeric(batter_milestones_50_odds)) |>
    mutate(market_name = "Batter Milestones") |>
    mutate(agency = "Bet365")
  
  batter_milestones_70 <-
    tibble(player = batter_milestones_players,
           team = batter_milestones_teams,
           line = 69.5,
           over_price = as.numeric(batter_milestones_70_odds)) |>
    mutate(market_name = "Batter Milestones") |>
    mutate(agency = "Bet365")
  
  # Combine
  batter_milestones <-
    bind_rows(batter_milestones_10, batter_milestones_20, batter_milestones_30, batter_milestones_50, batter_milestones_70)
  
  #=============================================================================
  # Combine Lines and milestones
  #=============================================================================
  
  # Get Match Name
  match_name <-
    scraped_file |> 
    read_html() |>
    html_node(".sph-EventWrapper_Label ") |>
    html_text() |> 
    str_remove_all(" \\- .*")
  
  # Combine all tables
  batter_all_match_runs <-
    bind_rows(batter_match_runs, batter_milestones) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Runs") |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  # Return
  return(batter_all_match_runs)
}

# Map Over all html files
list_of_player_runs <- map(scraped_files_player, get_player_runs)

# Create function to fix the player names
fix_player_names <- function(player_name_vector) {
  player_name_vector <- case_when(
    str_detect(player_name_vector, "ADS Fletcher") ~ "Andre Fletcher",
    str_detect(player_name_vector, "BA King") ~ "Brandon King",
    str_detect(player_name_vector, "E Lewis") ~ "Evin Lewis",
    str_detect(player_name_vector, "F Zaman") ~ "Fakhar Zaman",
    str_detect(player_name_vector, "JP Greaves") ~ "Jamie Greaves",
    str_detect(player_name_vector, "KR Mayers") ~ "Kyle Mayers",
    str_detect(player_name_vector, "RR Rossouw") ~ "Rilee Rossouw",
    str_detect(player_name_vector, "T Bishop") ~ "Teddy Bishop",
    str_detect(player_name_vector, "K James") ~ "Kofi James",
    str_detect(player_name_vector, "SW Billings") ~ "Sam Billings",
    str_detect(player_name_vector, "SO Hetmyer") ~ "Shimron Hetmyer",
    str_detect(player_name_vector, "JJ Roy") ~ "Jason Roy",
    str_detect(player_name_vector, "M Deyal") ~ "Mark Deyal",
    str_detect(player_name_vector, "N Pooran") ~ "Nicholas Pooran",
    str_detect(player_name_vector, "T Stubbs") ~ "Tristan Stubbs",
    str_detect(player_name_vector, "TH David") ~ "Tim David",
    str_detect(player_name_vector, "M Nandu") ~ "Mikyle Nandu",
    str_detect(player_name_vector, "SD Hope") ~ "Shai Hope",
    str_detect(player_name_vector, "R Gurbaz") ~ "Rahmanullah Gurbaz",
    str_detect(player_name_vector, "Q de Kock") ~ "Quinton de Kock",
    str_detect(player_name_vector, "RRS Cornwall") ~ "Rahkeem Cornwall",
    str_detect(player_name_vector, "DA Miller") ~ "David Miller",
    str_detect(player_name_vector, "A Athanaze") ~ "Alick Athanaze",
    str_detect(player_name_vector, "F du Plessis") ~ "Faf du Plessis",
    str_detect(player_name_vector, "J Charles") ~ "Johnson Charles",
    str_detect(player_name_vector, "TL Seifert") ~ "Tim Seifert",
    str_detect(player_name_vector, "M Louis") ~ "Mikyle Louis",
    str_detect(player_name_vector, "PBB Rajapaksa") ~ "Bhanuka Rajapaksa",
    TRUE ~ player_name_vector
  )
}

# Combine into a df
player_runs <-
  bind_rows(list_of_player_runs) |> 
  mutate(player = fix_player_names(player)) |> 
  rename(market = market_name, player_name = player, player_team = team)

# Output as a csv
write_csv(player_runs, "Data/T20s/Internationals/scraped_odds/bet365_player_runs.csv")
