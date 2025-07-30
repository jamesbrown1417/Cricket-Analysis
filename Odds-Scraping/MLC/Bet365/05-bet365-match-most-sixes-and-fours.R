# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Source the fix team names function
source("Functions/fix_team_names.R")

# Read scraped HTML from the BET365_HTML Folder
scraped_files_boundaries <- list.files("Odds-Scraping/MLC/Bet365/HTML", full.names = TRUE, pattern = "body_html_match")

# Main Function
get_match_boundaries <- function(scraped_file) {
  # Get Markets
  bet365_boundary_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_boundary_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text ") |>
    html_text()
  
  # Get Match Name
  match_name <-
    scraped_file |> 
    read_html() |>
    html_node(".sph-EventWrapper_Label ") |>
    html_text() |> 
    str_remove_all(" \\- .*")
  
  # Initialize results list
  results <- list()
  
  #=============================================================================
  # Most Match Sixes (Head to Head)
  #=============================================================================
  
  # Get index for node with text "Most Match Sixes"
  most_sixes_index <- which(market_names == "Most Match Sixes")
  
  # Extract team names and odds for Most Match Sixes
  if(length(most_sixes_index) > 0) {
    # Get team names
    most_sixes_teams <-
      bet365_boundary_markets[[most_sixes_index]] |> 
      html_elements(".gl-ParticipantBorderless_Name") |>
      html_text()
    
    # Get odds
    most_sixes_odds <-
      bet365_boundary_markets[[most_sixes_index]] |> 
      html_elements(".gl-ParticipantBorderless_Odds") |>
      html_text()
    
    # Create Most Match Sixes Table
    most_sixes_table <-
      tibble(
        team = most_sixes_teams,
        price = as.numeric(most_sixes_odds)
      ) |>
      mutate(
        market = "Team To Score Most Sixes",
        agency = "Bet365",
        match = match_name,
        team = fix_team_names_mlc(team)
      ) |> 
      separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
      mutate(home_team = fix_team_names_mlc(home_team),
    away_team = fix_team_names_mlc(away_team)) |> 
      relocate(match, market, home_team, away_team, .before = team)
    
    results$most_sixes <- most_sixes_table
  }
  
  #=============================================================================
  # Most Match Fours (Head to Head)
  #=============================================================================
  
  # Get index for node with text "Most Match Fours"
  most_fours_index <- which(market_names == "Most Match Fours")
  
  # Extract team names and odds for Most Match Fours
  if(length(most_fours_index) > 0) {
    # Get team names
    most_fours_teams <-
      bet365_boundary_markets[[most_fours_index]] |> 
      html_elements(".gl-ParticipantBorderless_Name") |>
      html_text()
    
    # Get odds
    most_fours_odds <-
      bet365_boundary_markets[[most_fours_index]] |> 
      html_elements(".gl-ParticipantBorderless_Odds") |>
      html_text()
    
    # Create Most Match Fours Table
    most_fours_table <-
      tibble(
        team = most_fours_teams,
        price = as.numeric(most_fours_odds)
      ) |>
      mutate(
        market = "Team To Score Most Fours",
        agency = "Bet365",
        match = match_name,
        team = fix_team_names_mlc(team)) |> 
          separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
          mutate(home_team = fix_team_names_mlc(home_team),
        away_team = fix_team_names_mlc(away_team)) |> 
          relocate(match, market, home_team, away_team, .before = team) |> 
      relocate(match, market, home_team, away_team, .before = team)
    
    results$most_fours <- most_fours_table
  }
  
  # Combine results
  if(length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(NULL)
  }
}

# Get safe version
get_match_boundaries_safe <- safely(get_match_boundaries)

# Map Over all html files
list_of_boundaries <- map(scraped_files_boundaries, get_match_boundaries_safe)

# Keep only elements with NULL error
list_of_boundaries <- 
  list_of_boundaries |> 
  keep(~is.null(.x$error)) |> 
  map(~.x$result) |>
  keep(~!is.null(.x))

# Combine into df
most_match_boundaries <- bind_rows(list_of_boundaries)

# Split into separate dataframes
most_match_sixes <- most_match_boundaries |> filter(market == "Team To Score Most Sixes")
most_match_fours <- most_match_boundaries |> filter(market == "Team To Score Most Fours")

# Output as separate csvs
if(nrow(most_match_sixes) > 0) {
  write_csv(most_match_sixes, "Data/T20s/Major League Cricket/scraped_odds/bet365_most_match_sixes.csv")
}

if(nrow(most_match_fours) > 0) {
  write_csv(most_match_fours, "Data/T20s/Major League Cricket/scraped_odds/bet365_most_match_fours.csv")
}