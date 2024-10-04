# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_boundaries <- list.files("Odds-Scraping/WT20I/Bet365/HTML", full.names = TRUE, pattern = "body_html_match")

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
  
  #=============================================================================
  # Total Match Fours and Sixes
  #=============================================================================
  
  # Get index for node with text "Total Match Fours"
  total_boundaries_index <- which(market_names == "Match Totals")
  
  # Get teams or relevant context (if applicable)
  total_boundaries_teams <-
    bet365_boundary_markets[[total_boundaries_index]] |> 
    html_elements(".srb-ParticipantLabel_Name") |>
    html_text()
  
  # Get Over Node Index for Total Match Fours
  total_boundaries_cols <-
    bet365_boundary_markets[[total_boundaries_index]] |>
    html_elements(".gl-Market_General")
  
  total_boundaries_over_index <- which(str_detect(total_boundaries_cols |> html_text(), "Over"))
  
  # Get Over Lines
  total_boundaries_over_lines <-
    total_boundaries_cols[[total_boundaries_over_index]] |>
    html_elements(".srb-ParticipantCenteredStackedMarketRow_Handicap") |>
    html_text()
  
  # Get Over Odds
  total_boundaries_over_odds <-
    total_boundaries_cols[[total_boundaries_over_index]] |>
    html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |>
    html_text()
  
  # Get Under Node Index for Total Match Fours
  total_boundaries_under_index <- which(str_detect(total_boundaries_cols |> html_text(), "Under"))
  
  # Get Under Odds
  total_boundaries_under_odds <-
    total_boundaries_cols[[total_boundaries_under_index]] |>
    html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |>
    html_text()
  
  # Create Total Match Boundaries Table
  total_boundaries_table <-
    tibble(market = total_boundaries_teams,
           line = as.numeric(total_boundaries_over_lines),
           over_price = as.numeric(total_boundaries_over_odds),
           under_price = as.numeric(total_boundaries_under_odds)) |>
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
  total_boundaries_table <-
    total_boundaries_table |>
    mutate(match = match_name) |> 
    relocate(match, .before = market)
  
  # Return
  return(total_boundaries_table)
}

# Get safe version
get_match_boundaries_safe <- safely(get_match_boundaries)

# Map Over all html files
list_of_boundaries <- map(scraped_files_boundaries, get_match_boundaries_safe)

# Keep only elements with NULL error
list_of_boundaries <- 
  list_of_boundaries |> 
  keep(~is.null(.x$error)) |> 
  map(~.x$result)

# Combine into dfs
total_match_fours <-
  bind_rows(list_of_boundaries) |> 
  filter(market == "Fours") |> 
  mutate(market = "Total Match Fours")

total_match_sixes <-
  bind_rows(list_of_boundaries) |> 
  filter(market == "Sixes") |> 
  mutate(market = "Total Match Sixes")

total_match_boundaries <-
  bind_rows(list_of_boundaries) |> 
  filter(market == "Boundaries") |> 
  mutate(market = "Total Match Boundaries")

# Output as a csv
write_csv(total_match_fours, "Data/T20s/WT20I/scraped_odds/bet365_total_match_fours.csv")
write_csv(total_match_sixes, "Data/T20s/WT20I/scraped_odds/bet365_total_match_sixes.csv")
write_csv(total_match_boundaries, "Data/T20s/WT20I/scraped_odds/bet365_total_match_boundaries.csv")