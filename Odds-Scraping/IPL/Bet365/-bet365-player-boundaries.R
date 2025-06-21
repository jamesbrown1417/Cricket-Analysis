# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Fix team names function


# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Odds-Scraping/BBL/Bet365/HTML", full.names = TRUE, pattern = "player")

# Main Function
get_player_boundaries <- function(scraped_file) {
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
  # Batter Match Fours
  #=============================================================================
  
  # Get index for node with text "Batter Total Match Fours"
  batter_match_fours_index <- which(market_names == "Batter Total Match Fours")
  
  # Get Player Names from node
  batter_match_fours_players <-
    bet365_player_markets[[batter_match_fours_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Get Player Teams from node
  batter_match_fours_teams <-
    bet365_player_markets[[batter_match_fours_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    html_text()
  
  # Get Over Node Index
  batter_match_fours_cols <-
    bet365_player_markets[[batter_match_fours_index]] |>
    html_elements(".gl-Market_General")
  
  batter_match_fours_over_index <- which(str_detect(batter_match_fours_cols |> html_text(), "Over"))
  
  # Get Over Lines
  batter_match_fours_over_lines <-
    batter_match_fours_cols[[batter_match_fours_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  batter_match_fours_over_odds <-
    batter_match_fours_cols[[batter_match_fours_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  batter_match_fours_under_index <- which(str_detect(batter_match_fours_cols |> html_text(), "Under"))
  
  # Get Under Odds
  batter_match_fours_under_odds <-
    batter_match_fours_cols[[batter_match_fours_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Batter Match fours Table
  batter_match_fours <-
    tibble(player = batter_match_fours_players,
           team = batter_match_fours_teams,
           line = as.numeric(batter_match_fours_over_lines),
           over_price = as.numeric(batter_match_fours_over_odds),
           under_price = as.numeric(batter_match_fours_under_odds)) |>
    mutate(market_name = "Batter Match fours") |>
    mutate(agency = "Bet365")
  
  #=============================================================================
  # Batter Match Sixes
  #=============================================================================
  
  # Get index for node with text "Batter Total Match Sixes"
  batter_match_sixes_index <- which(market_names == "Batter Total Match Sixes")
  
  # Get Player Names from node
  batter_match_sixes_players <-
    bet365_player_markets[[batter_match_sixes_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Name") |>
    html_text()
  
  # Get Player Teams from node
  batter_match_sixes_teams <-
    bet365_player_markets[[batter_match_sixes_index]] |>
    html_elements(".srb-ParticipantLabelWithTeam_Team") |>
    html_text()
  
  # Get Over Node Index
  batter_match_sixes_cols <-
    bet365_player_markets[[batter_match_sixes_index]] |>
    html_elements(".gl-Market_General")
  
  batter_match_sixes_over_index <- which(str_detect(batter_match_sixes_cols |> html_text(), "Over"))
  
  # Get Over Lines
  batter_match_sixes_over_lines <-
    batter_match_sixes_cols[[batter_match_sixes_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
    html_text()
  
  # Get Over Odds
  batter_match_sixes_over_odds <-
    batter_match_sixes_cols[[batter_match_sixes_over_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Get Under Node Index
  batter_match_sixes_under_index <- which(str_detect(batter_match_sixes_cols |> html_text(), "Under"))
  
  # Get Under Odds
  batter_match_sixes_under_odds <-
    batter_match_sixes_cols[[batter_match_sixes_under_index]] |>
    html_elements(".gl-ParticipantCenteredStacked_Odds") |>
    html_text()
  
  # Create Batter Match sixes Table
  batter_match_sixes <-
    tibble(player = batter_match_sixes_players,
           team = batter_match_sixes_teams,
           line = as.numeric(batter_match_sixes_over_lines),
           over_price = as.numeric(batter_match_sixes_over_odds),
           under_price = as.numeric(batter_match_sixes_under_odds)) |>
    mutate(market_name = "Batter Match sixes") |>
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
  
  # Combine all tables
  batter_all_match_boundaries <-
    bind_rows(batter_match_fours, batter_match_sixes) |> 
    arrange(player, line, over_price) |> 
    mutate(match = match_name) |> 
    relocate(match, .before = player)
  
  # Return
  return(batter_all_match_boundaries)
}

# Get safe version
get_player_boundaries_safe <- safely(get_player_boundaries)

# Map Over all html files
list_of_player_boundaries <- map(scraped_files_player, get_player_boundaries_safe)

# Keep only elements with NULL error
list_of_player_boundaries <- 
  list_of_player_boundaries |> 
  keep(~is.null(.x$error)) |> 
  map(~.x$result) |> 
  bind_rows()

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
    str_detect(player_name_vector, "BR McDermott") ~ "Ben McDermott",
    str_detect(player_name_vector, "CP Jewell") ~ "Caleb Jewell",
    str_detect(player_name_vector, "J Fraser-McGurk" ) ~ "Jake Fraser-McGurk",
    str_detect(player_name_vector, "J Brown") ~ "Josh Brown",
    str_detect(player_name_vector, "L Evans") ~ "Laurie Evans",
    str_detect(player_name_vector, "JR Philippe") ~ "Josh Philippe",
    str_detect(player_name_vector, "M Bryant") ~ "Max Bryant",
    str_detect(player_name_vector, "M Labuschagne") ~ "Marnus Labuschagne",
    str_detect(player_name_vector, "M Renshaw") ~ "Matt Renshaw",
    str_detect(player_name_vector, "M Henriques") ~ "Moises Henriques",
    str_detect(player_name_vector, "M Stoinis") ~ "Marcus Stoinis",
    str_detect(player_name_vector, "M Louis") ~ "Mikyle Louis",
    str_detect(player_name_vector, "PBB Rajapaksa") ~ "Bhanuka Rajapaksa",
    str_detect(player_name_vector, "K Alleyne") ~ "Kadeem Alleyne",
    str_detect(player_name_vector, "MM Ali") ~ "Moeen Ali",
    str_detect(player_name_vector, "B Webster") ~ "Beau Webster",
    str_detect(player_name_vector, "KR Patterson") ~ "Kurtis Patterson",
    str_detect(player_name_vector, "JM Vince") ~ "James Vince",
    str_detect(player_name_vector, "MS Wade") ~ "Matthew Wade",
    str_detect(player_name_vector, "C Munro") ~ "Colin Munro",
    str_detect(player_name_vector, "CA Lynn") ~ "Chris Lynn",
    str_detect(player_name_vector, "DJM Short") ~ "D'Arcy Short",
    str_detect(player_name_vector, "JJ Peirson") ~ "Jimmy Peirson",
    str_detect(player_name_vector, "MW Short") ~ "Matt Short",
    str_detect(player_name_vector, "NA McSweeney") ~ "Nathan McSweeney",
    str_detect(player_name_vector, "C Connolly") ~ "Cooper Connolly",
    str_detect(player_name_vector, "KK Jennings") ~ "Keaton Jennings",
    str_detect(player_name_vector, "BM Duckett") ~ "Ben Duckett",
    str_detect(player_name_vector, "J Edwards") ~ "Jack Edwards",
    str_detect(player_name_vector, "MP Stoinis") ~ "Marcus Stoinis",
    str_detect(player_name_vector, "SB Harper") ~ "Sam Harper",
    TRUE ~ player_name_vector
  )
}

# Combine into a df
player_boundaries <-
  bind_rows(list_of_player_boundaries) |> 
  mutate(player = fix_player_names(player)) |> 
  rename(market = market_name, player_name = player, player_team = team)

# Output as a csv
write_csv(player_boundaries, "Data/T20s/Big Bash/scraped_odds/bet365_player_boundaries.csv")