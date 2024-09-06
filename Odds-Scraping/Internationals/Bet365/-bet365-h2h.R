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
  scraped_file <- list.files("Odds-Scraping/Internationals/Bet365/HTML", full.names = TRUE, pattern = "h2h")[[1]]
  
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
write_csv(bet365_h2h, "Data/T20s/Internationals/scraped_odds/bet365_h2h.csv")