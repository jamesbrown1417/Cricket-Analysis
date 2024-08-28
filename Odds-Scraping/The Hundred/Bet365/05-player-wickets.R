# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Odds-Scraping/The Hundred/Bet365/HTML", full.names = TRUE, pattern = "player")

# Main Function
get_player_wickets <- function(scraped_file) {
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
  # Bowler Total Match Wickets
  #=============================================================================
  
  # Get index for node with text "Bowler Total Match Wickets"
 bowler_match_wickets_index <- which(market_names == "Bowler Total Match Wickets")
  
  # Get Player Names from node
 bowler_match_wickets_players <-
    bet365_player_markets[[batter_match_runs_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Get Player Teams from node
 bowler_match_wickets_teams <-
    bet365_player_markets[[bowler_match_wickets_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    html_text()
  
  # Get Over Node Index
 bowler_match_wickets_cols <-
    bet365_player_markets[[bowler_match_wickets_index]] |>
    html_elements(".gl-Market_General")
  
 bowler_match_wickets_over_index <- which(str_detect(bowler_match_wickets_cols |> html_text(), "Over"))
  
  # Get Over Lines
 bowler_match_wickets_over_lines <-
   bowler_match_wickets_cols[[bowler_match_wickets_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
 bowler_match_wickets_over_odds <-
   bowler_match_wickets_cols[[bowler_match_wickets_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
 bowler_match_wickets_under_index <- which(str_detect(bowler_match_wickets_cols |> html_text(), "Under"))
  
  # Get Under Odds
 bowler_match_wickets_under_odds <-
   bowler_match_wickets_cols[[bowler_match_wickets_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Bowler Total Match Wickets Table
 bowler_match_wickets <-
    tibble(player =bowler_match_wickets_players,
           team = bowler_match_wickets_teams,
           line = as.numeric(bowler_match_wickets_over_lines),
           over_price = as.numeric(bowler_match_wickets_over_odds),
           under_price = as.numeric(bowler_match_wickets_under_odds)) |>
    mutate(market_name = "Bowler Total Match Wickets") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Bowler Milestones
  #=============================================================================
  
  # Get index for node with text "Bowler Milestones"
  bowler_milestones_index <- which(market_names == "Bowler Milestones")
  
  # Get Player Names from node
  bowler_milestones_players <-
    bet365_player_markets[[bowler_milestones_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Get Player Teams from node
  bowler_milestones_teams <-
    bet365_player_markets[[bowler_milestones_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    html_text()
  
  # Get 1+ Wickets Node Index
  bowler_milestones_cols <-
    bet365_player_markets[[bowler_milestones_index]] |>
    html_elements(".gl-Market_General")
  
  bowler_milestones_1_index <- which(str_detect(bowler_milestones_cols |> html_text(), "1\\+"))
  bowler_milestones_2_index <- which(str_detect(bowler_milestones_cols |> html_text(), "2\\+"))
  bowler_milestones_3_index <- which(str_detect(bowler_milestones_cols |> html_text(), "3\\+"))
  bowler_milestones_4_index <- which(str_detect(bowler_milestones_cols |> html_text(), "4\\+"))
  bowler_milestones_5_index <- which(str_detect(bowler_milestones_cols |> html_text(), "5\\+"))
  
  # Get 1+ Wickets Odds
  bowler_milestones_1_odds <-
    bowler_milestones_cols[[bowler_milestones_1_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 2+ Wickets Odds
  bowler_milestones_2_odds <-
    bowler_milestones_cols[[bowler_milestones_2_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 3+ Wickets Odds
  bowler_milestones_3_odds <-
    bowler_milestones_cols[[bowler_milestones_3_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 4+ Wickets Odds
  bowler_milestones_4_odds <-
    bowler_milestones_cols[[bowler_milestones_4_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Get 5+ Wickets Odds
  bowler_milestones_5_odds <-
    bowler_milestones_cols[[bowler_milestones_5_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  # Create Bowler Milestones Tables
  bowler_milestones_1 <-
    tibble(player = bowler_milestones_players,
           team = bowler_milestones_teams,
           line = 0.5,
           over_price = as.numeric(bowler_milestones_1_odds)) |>
    mutate(market_name = "Bowler Milestones") |>
    mutate(agency = "Bet365")
  
  bowler_milestones_2 <-
    tibble(player = bowler_milestones_players,
           team = bowler_milestones_teams,
           line = 1.5,
           over_price = as.numeric(bowler_milestones_2_odds)) |>
    mutate(market_name = "Bowler Milestones") |>
    mutate(agency = "Bet365")
  
  bowler_milestones_3 <-
    tibble(player = bowler_milestones_players,
           team = bowler_milestones_teams,
           line = 2.5,
           over_price = as.numeric(bowler_milestones_3_odds)) |>
    mutate(market_name = "Bowler Milestones") |>
    mutate(agency = "Bet365")
  
  bowler_milestones_4 <-
    tibble(player = bowler_milestones_players,
           team = bowler_milestones_teams,
           line = 3.5,
           over_price = as.numeric(bowler_milestones_4_odds)) |>
    mutate(market_name = "Bowler Milestones") |>
    mutate(agency = "Bet365")
  
  bowler_milestones_5 <-
    tibble(player = bowler_milestones_players,
           team = bowler_milestones_teams,
           line = 4.5,
           over_price = as.numeric(bowler_milestones_5_odds)) |>
    mutate(market_name = "Bowler Milestones") |>
    mutate(agency = "Bet365")
  
  # Combine
  bowler_milestones <-
    bind_rows(bowler_milestones_1, bowler_milestones_2, bowler_milestones_3, bowler_milestones_4, bowler_milestones_5)
  
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
  bowler_all_match_wickets <-
    bind_rows(bowler_match_wickets, bowler_milestones) |> 
    arrange(player, line, over_price) |> 
    mutate(market_name = "Player Wickets") |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  # Return
  return(bowler_all_match_wickets)
}

# Map Over all html files
list_of_player_wickets <- map(scraped_files_player, get_player_wickets)

# Combine into a df
player_wickets <- bind_rows(list_of_player_wickets)

# Output as a csv
write_csv(player_wickets, "Data/T20s/The Hundred/scraped_odds/bet365_player_wickets.csv")   
