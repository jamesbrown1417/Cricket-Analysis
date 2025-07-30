# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_first_over <- list.files("Odds-Scraping/MLC/Bet365/HTML", full.names = TRUE, pattern = "first_over")

# Main Function
get_first_over_runs <- function(scraped_file) {
  # Get Markets
  bet365_first_over_markets <-
    read_html(scraped_file) |>
    html_nodes(".gl-MarketGroupPod")
  
  # Market Names
  market_names <-
    bet365_first_over_markets |>
    html_elements(".cm-MarketGroupWithIconsButton_Text ") |>
    html_text()
  
  #=============================================================================
  # First Over Runs - Team
  #=============================================================================
  
  # Get index for node with text "1st Over of Match - Total Runs - Team"
  first_over_team_index <- which(market_names == "1st Over of Match - Total Runs - Team")
  
  # Get teams
  first_over_runs_teams <-
    bet365_first_over_markets[[first_over_team_index]] |>
    html_elements(".srb-ParticipantLabel") |>
    html_text()
  
  # Get Over Node Index
  first_over_runs_cols <-
    bet365_first_over_markets[[first_over_team_index]] |>
    html_elements(".gl-Market_General")
  
  first_over_runs_over_index <- which(str_detect(first_over_runs_cols |> html_text(), "Over"))
  
  # Get Over Lines
  first_over_runs_over_lines <-
    first_over_runs_cols[[first_over_runs_over_index]] |>
    html_elements(".srb-ParticipantCenteredStackedMarketRow_Handicap") |>
    html_text()

  # Get Over Odds
  first_over_runs_over_odds <-
    first_over_runs_cols[[first_over_runs_over_index]] |>
    html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |>
    html_text()
  
  # Get Under Node Index
  first_over_runs_under_index <- which(str_detect(first_over_runs_cols |> html_text(), "Under"))
  
  # Get Under Odds
  first_over_runs_under_odds <-
    first_over_runs_cols[[first_over_runs_under_index]] |>
    html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |>
    html_text()
  
  # Create First Over Runs Table
  first_over_team_runs <-
    tibble(team = first_over_runs_teams,
           line = as.numeric(first_over_runs_over_lines),
           over_price = as.numeric(first_over_runs_over_odds),
           under_price = as.numeric(first_over_runs_under_odds)) |>
    mutate(market_name = "First Over Runs - Team") |>
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
  first_over_team_runs <-
    first_over_team_runs |>
    mutate(match = match_name) |> 
    relocate(match, .before = team)
  
  # Return
  return(first_over_team_runs)
}

# Get safe version
get_first_over_runs_safe <- safely(get_first_over_runs)

# Map Over all html files
list_of_first_over_runs <- map(scraped_files_first_over, get_first_over_runs_safe)

# Keep only elements with NULL error
list_of_first_over_runs <- 
  list_of_first_over_runs |> 
  keep(~is.null(.x$error)) |> 
  map(~.x$result) |> 
  bind_rows()

# Combine into a df
first_over_runs_all <-
  bind_rows(list_of_first_over_runs) |> 
  rename(market = market_name)

# Output as a csv
write_csv(first_over_runs_all, "Data/T20s/Major League Cricket/scraped_odds/bet365_first_over_runs.csv")
