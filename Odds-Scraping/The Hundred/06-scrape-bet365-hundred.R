# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

get_head_to_head <- function() {
  
  # Read scraped HTML from the BET365_HTML Folder
  scraped_file <- list.files("Odds-Scraping/The Hundred/Bet365/HTML", full.names = TRUE, pattern = "h2h")[[1]]
  
  # Get Teams
  bet365_teams <-
    read_html(scraped_file) |> 
    html_nodes(".rcl-ParticipantFixtureDetailsTeam_TeamName") |>
    html_text()
  
  # Get H2H Odds
  bet365_h2h_odds <-
    read_html(scraped_file) |> 
    html_nodes(".sgl-ParticipantOddsOnly80_Odds") |> 
    html_text()
  
  #===============================================================================
  # Create head to head table----------------------------------------------------#
  #===============================================================================
  
  # Get Home teams - Odd elements
  home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
  home_odds <- bet365_h2h_odds[1:length(home_teams)]
  
  # Get only positions of elements of home odds that are numeric
  home_odds_to_keep <- which(str_detect(home_odds, "^\\d+\\.\\d+$"))
  
  home_teams <- home_teams[home_odds_to_keep]
  home_odds <- home_odds[home_odds_to_keep]
  
  home_h2h <- tibble(home_teams, home_odds)
  
  # Get Away teams - Even elements
  away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
  away_odds <- bet365_h2h_odds[length(away_teams) + 1:length(away_teams)]
  
  # Get only positions of elements of away odds that are numeric
  away_odds_to_keep <- which(str_detect(away_odds, "^\\d+\\.\\d+$"))
  
  away_teams <- away_teams[away_odds_to_keep]
  away_odds <- away_odds[away_odds_to_keep]
  
  away_h2h <- tibble(away_teams, away_odds)
  
  # Combine together into one table
  bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365")
  
  # Return
  return(bet365_h2h)
}

# Run function
bet365_h2h <- get_head_to_head()

# Write to csv
write_csv(bet365_h2h, "Data/T20s/The Hundred/scraped_odds/bet365_h2h.csv")

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Odds-Scraping/The Hundred/Bet365/HTML", full.names = TRUE, pattern = "player")

  
# Main Function
get_player_props <- function(scraped_file) {
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
}
