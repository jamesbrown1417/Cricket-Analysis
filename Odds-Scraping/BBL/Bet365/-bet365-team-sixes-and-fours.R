# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_team_boundaries <- list.files("Odds-Scraping/BBL/Bet365/HTML", full.names = TRUE, pattern = "body_html_team")

# Main Function
get_team_boundaries <- function(scraped_file) {
  # Get Markets
  bet365_boundary_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_boundary_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text ") |>
    html_text()
  
  #=============================================================================
  # Total Team Fours
  #=============================================================================
  
  # Get index for node with text "Team Total Match Fours"
  team_boundaries_index <- which(market_names == "Team Total Match Fours")
  
  # Get teams or relevant context (if applicable)
  team_boundaries_teams <-
    bet365_boundary_markets[[team_boundaries_index]] |> 
    html_elements(".srb-ParticipantLabel_Name") |>
    html_text()
  
  # Get Over Node Index for Total Match Fours
  team_boundaries_cols <-
    bet365_boundary_markets[[team_boundaries_index]] |>
    html_elements(".gl-Market_General")
  
  team_boundaries_over_index <- which(str_detect(team_boundaries_cols |> html_text(), "Over"))
  
  # Get Over Lines
  team_boundaries_over_lines <-
    team_boundaries_cols[[team_boundaries_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  team_boundaries_over_odds <-
    team_boundaries_cols[[team_boundaries_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index for Total Match Fours
  team_boundaries_under_index <- which(str_detect(team_boundaries_cols |> html_text(), "Under"))
  
  # Get Under Odds
  team_boundaries_under_odds <-
    team_boundaries_cols[[team_boundaries_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Total Match Boundaries Table
  team_fours_table <-
    tibble(team = team_boundaries_teams,
           line = as.numeric(team_boundaries_over_lines),
           over_price = as.numeric(team_boundaries_over_odds),
           under_price = as.numeric(team_boundaries_under_odds)) |>
    mutate(agency = "Bet365") |> 
    mutate(market = "Total Team Fours")
  
  #=============================================================================
  # Total Team Sixes
  #=============================================================================
  
  # Get index for node with text "Team Total Match Sixes"
  team_boundaries_index <- which(market_names == "Team Total Match Sixes")
  
  # Get teams or relevant context (if applicable)
  team_boundaries_teams <-
    bet365_boundary_markets[[team_boundaries_index]] |> 
    html_elements(".srb-ParticipantLabel_Name") |>
    html_text()
  
  # Get Over Node Index for Total Match Fours
  team_boundaries_cols <-
    bet365_boundary_markets[[team_boundaries_index]] |>
    html_elements(".gl-Market_General")
  
  team_boundaries_over_index <- which(str_detect(team_boundaries_cols |> html_text(), "Over"))
  
  # Get Over Lines
  team_boundaries_over_lines <-
    team_boundaries_cols[[team_boundaries_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  team_boundaries_over_odds <-
    team_boundaries_cols[[team_boundaries_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index for Total Match Fours
  team_boundaries_under_index <- which(str_detect(team_boundaries_cols |> html_text(), "Under"))
  
  # Get Under Odds
  team_boundaries_under_odds <-
    team_boundaries_cols[[team_boundaries_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Total Match Boundaries Table
  team_sixes_table <-
    tibble(team = team_boundaries_teams,
           line = as.numeric(team_boundaries_over_lines),
           over_price = as.numeric(team_boundaries_over_odds),
           under_price = as.numeric(team_boundaries_under_odds)) |>
    mutate(agency = "Bet365") |> 
    mutate(market = "Total Team Sixes")
  
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
  
  # Add to the tables
  team_boundaries_table <-
    team_fours_table |>
    bind_rows(team_sixes_table) |>
    mutate(match = match_name) |> 
    relocate(match, .before = market)
  
  # Return
  return(team_boundaries_table)
}

# Get safe version
get_team_boundaries_safe <- safely(get_team_boundaries)

# Map Over all html files
list_of_boundaries <- map(scraped_files_team_boundaries, get_team_boundaries_safe)

# Keep only elements with NULL error
list_of_boundaries <- 
  list_of_boundaries |> 
  keep(~is.null(.x$error)) |> 
  map(~.x$result)

# Combine into dfs
total_team_fours <-
  bind_rows(list_of_boundaries) |> 
  filter(market == "Total Team Fours")

total_team_sixes <-
  bind_rows(list_of_boundaries) |> 
  filter(market == "Total Team Sixes")

# Output as a csv
write_csv(total_team_fours, "Data/T20s/Big Bash/scraped_odds/bet365_team_fours.csv")
write_csv(total_team_sixes, "Data/T20s/Big Bash/scraped_odds/bet365_team_sixes.csv")