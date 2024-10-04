# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_fofw <- list.files("Odds-Scraping/WT20I/Bet365/HTML", full.names = TRUE, pattern = "body_html_team")

# Main Function
get_fofw_runs <- function(scraped_file) {
  # Get Markets
  bet365_fofw_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_fofw_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text ") |>
    html_text()
  
  #=============================================================================
  # Fall of First Wicket
  #=============================================================================
  
  # Get index for node with text "Team - Opening Partnership Total"
  fofw_team_index <- which(market_names == "Team - Opening Partnership Total")
  
  # Get teams
  fofw_runs_teams <-
    bet365_fofw_markets[[fofw_team_index]] |> 
    html_elements(".srb-ParticipantLabel_Name ") |>
    html_text()
  
  # Get Over Node Index
  fofw_runs_cols <-
    bet365_fofw_markets[[fofw_team_index]] |>
    html_elements(".gl-Market_General")
  
  fofw_runs_over_index <- which(str_detect(fofw_runs_cols |> html_text(), "Over"))
  
  # Get Over Lines
  fofw_runs_over_lines <-
    fofw_runs_cols[[fofw_runs_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  fofw_runs_over_odds <-
    fofw_runs_cols[[fofw_runs_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  fofw_runs_under_index <- which(str_detect(fofw_runs_cols |> html_text(), "Under"))
  
  # Get Under Odds
  fofw_runs_under_odds <-
    fofw_runs_cols[[fofw_runs_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create First Over Runs Table
  fofw_team_runs <-
    tibble(team = fofw_runs_teams,
           line = as.numeric(fofw_runs_over_lines),
           over_price = as.numeric(fofw_runs_over_odds),
           under_price = as.numeric(fofw_runs_under_odds)) |>
    mutate(market_name = "Fall of 1st Wicket - Team") |>
    mutate(agency = "Bet365")
  
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
  fofw_team_runs <-
    fofw_team_runs |>
    mutate(match = match_name) |> 
    relocate(match, .before = team)
  
  # Return
  return(fofw_team_runs)
}

# Map Over all html files
list_of_fofw_runs <- map(scraped_files_fofw, get_fofw_runs)

# Combine into a df
fofw_runs_all <-
  bind_rows(list_of_fofw_runs) |> 
  rename(market = market_name)

# Output as a csv
write_csv(fofw_runs_all, "Data/T20s/WT20I/scraped_odds/bet365_runs_at_first_wicket.csv")
