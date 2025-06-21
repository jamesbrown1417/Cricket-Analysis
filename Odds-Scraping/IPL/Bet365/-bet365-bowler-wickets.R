# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# Read scraped HTML from the BET365_HTML Folder
scraped_files_player <- list.files("Odds-Scraping/BBL/Bet365/HTML", full.names = TRUE, pattern = "player")

# Main Function
get_bowler_wickets <- function(scraped_file) {
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
  # Bowler Match Wickets
  #=============================================================================
  
  # Get index for node with text "Bowler Match Wickets"
  bowler_match_wickets_index <- which(market_names == "Bowler Total Match Wickets")
  
  # Get Player Names from node
  bowler_match_wickets_players <- 
    bet365_player_markets[[bowler_match_wickets_index]] |>
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
  
  # Create Bowler Match Wickets Table
  bowler_match_wickets <- 
    tibble(player = bowler_match_wickets_players,
           team = bowler_match_wickets_teams,
           line = as.numeric(bowler_match_wickets_over_lines),
           over_price = as.numeric(bowler_match_wickets_over_odds),
           under_price = as.numeric(bowler_match_wickets_under_odds)) |>
    mutate(market_name = "Bowler Match Wickets") |>
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
  
  # Get Wicket Milestone Node Indices
  bowler_milestones_cols <- 
    bet365_player_markets[[bowler_milestones_index]] |>
    html_elements(".gl-Market_General")
  bowler_milestones_1_index <- which(str_detect(bowler_milestones_cols |> html_text(), "1\\+"))
  bowler_milestones_2_index <- which(str_detect(bowler_milestones_cols |> html_text(), "2\\+"))
  bowler_milestones_3_index <- which(str_detect(bowler_milestones_cols |> html_text(), "3\\+"))
  bowler_milestones_4_index <- which(str_detect(bowler_milestones_cols |> html_text(), "4\\+"))
  
  # Get Milestone Odds
  bowler_milestones_1_odds <- 
    bowler_milestones_cols[[bowler_milestones_1_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  bowler_milestones_2_odds <- 
    bowler_milestones_cols[[bowler_milestones_2_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  bowler_milestones_3_odds <- 
    bowler_milestones_cols[[bowler_milestones_3_index]] |>
    html_elements(".gl-ParticipantOddsOnly_Odds") |>
    html_text()
  
  bowler_milestones_4_odds <- 
    bowler_milestones_cols[[bowler_milestones_4_index]] |>
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
  
  # Combine
  bowler_milestones <- 
    bind_rows(bowler_milestones_1, bowler_milestones_2, bowler_milestones_3, bowler_milestones_4)
  
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

# Get safe version
get_bowler_wickets_safe <- safely(get_bowler_wickets)

# Map Over all html files
list_of_player_wickets <- map(scraped_files_player, get_bowler_wickets_safe)

# Keep only elements with NULL error
list_of_player_wickets <- 
  list_of_player_wickets |> 
  keep(~is.null(.x$error)) |> 
  map(~.x$result) |> 
  bind_rows()

# Create function to fix the player names
fix_player_names <- function(player_name_vector) {
  player_name_vector <- case_when(
    str_detect(player_name_vector, "A Hosein") ~ "Akeal Hosein",
    str_detect(player_name_vector, "A Joseph") ~ "Alzarri Joseph",
    str_detect(player_name_vector, "C Brathwaite") ~ "Carlos Brathwaite",
    str_detect(player_name_vector, "D Drakes") ~ "Dominic Drakes",
    str_detect(player_name_vector, "F Ahmed") ~ "Fabian Allen",
    str_detect(player_name_vector, "I Udana") ~ "Isuru Udana",
    str_detect(player_name_vector, "J Holder") ~ "Jason Holder",
    str_detect(player_name_vector, "K Cottoy") ~ "Kevin Sinclair",
    str_detect(player_name_vector, "K Paul") ~ "Keemo Paul",
    str_detect(player_name_vector, "K Pierre") ~ "Khary Pierre",
    str_detect(player_name_vector, "M Nabi") ~ "Mohammad Nabi",
    str_detect(player_name_vector, "O McCoy") ~ "Obed McCoy",
    str_detect(player_name_vector, "R Shepherd") ~ "Romario Shepherd",
    str_detect(player_name_vector, "S Cottrell") ~ "Sheldon Cottrell",
    str_detect(player_name_vector, "S Narine") ~ "Sunil Narine",
    str_detect(player_name_vector, "T Shamsi") ~ "Tabraiz Shamsi",
    str_detect(player_name_vector, "W Hasaranga") ~ "Wanindu Hasaranga",
    str_detect(player_name_vector, "C Green") ~ "Chris Green",
    str_detect(player_name_vector, "A Nortje") ~ "Anrich Nortje",
    str_detect(player_name_vector, "A Nedd") ~ "Ashmead Nedd",
    str_detect(player_name_vector, "AS Joseph") ~ "Alzarri Joseph",
    str_detect(player_name_vector, "D Pretorius") ~ "Dwaine Pretorius",
    str_detect(player_name_vector, "KR Mayers") ~ "Kyle Mayers",
    str_detect(player_name_vector, "G Motie") ~ "Gudakesh Motie",
    str_detect(player_name_vector, "S Joseph") ~ "Shamar Joseph",
    str_detect(player_name_vector, "SK Springer") ~ "Shamar Springer",
    str_detect(player_name_vector, "N Ahmad") ~ "Noor Ahmad",
    str_detect(player_name_vector, "I Wasim") ~ "Imad Wasim",
    str_detect(player_name_vector, "M Amir") ~ "Mohammad Amir",
    str_detect(player_name_vector, "M Clarke") ~ "McKenny Clarke",
    str_detect(player_name_vector, "MM Ali") ~ "Moeen Ali",
    str_detect(player_name_vector, "JO Holder") ~ "Jason Holder",
    str_detect(player_name_vector, "I Tahir") ~ "Imran Tahir",
    str_detect(player_name_vector, "K Maharaj") ~ "Keshav Maharaj",
    str_detect(player_name_vector, "OC McCoy") ~ "Obed McCoy",
    str_detect(player_name_vector, "MM Theekshana") ~ "Maheesh Theekshana",
    str_detect(player_name_vector, "A Zampa") ~ "Adam Zampa",
    str_detect(player_name_vector, "B Stanlake") ~ "Billy Stanlake",
    str_detect(player_name_vector, "F O'Neil") ~ "Fergus O'Neil",
    str_detect(player_name_vector, "KW Richardson") ~ "Kane Richardson",
    str_detect(player_name_vector, "N Ellis") ~ "Nathan Ellis",
    str_detect(player_name_vector, "RP Meredith") ~ "Riley Meredith",
    str_detect(player_name_vector, "TS Rogers") ~ "Tom Rogers",
    str_detect(player_name_vector, "W Salamkheil") ~ "Waqar Salamkheil",
    str_detect(player_name_vector, "B Dwarshuis") ~ "Ben Dwarshuis",
    str_detect(player_name_vector, "H Kerr") ~ "Hayden Kerr",
    str_detect(player_name_vector, "JM Bird") ~ "Jackson Bird",
    str_detect(player_name_vector, "SA Abbott") ~ "Sean Abbott",
    str_detect(player_name_vector, "CJ Boyce") ~ "Cameron Boyce",
    str_detect(player_name_vector, "HTRY Thornton") ~ "Henry Thornton",
    str_detect(player_name_vector, "J Overton") ~ "Jamie Overton",
    str_detect(player_name_vector, "L Pope") ~ "Lloyd Pope",
    str_detect(player_name_vector, "MJ Swepson") ~ "Mitchell Swepson",
    str_detect(player_name_vector, "MP Kuhnemann") ~ "Matthew Kuhnemann",
    str_detect(player_name_vector, "PI Walter") ~ "Paul Walter",
    str_detect(player_name_vector, "X Bartlett") ~ "Xavier Bartlett",
    str_detect(player_name_vector, "Adam Zampa") ~ "Adam Zampa",
    str_detect(player_name_vector, "AJ Tye") ~ "Andrew Tye",
    str_detect(player_name_vector, "Fergus O'Neil") ~ "Fergus O'Neill",
    str_detect(player_name_vector, "JA Richardson") ~ "Jhye Richardson",
    str_detect(player_name_vector, "JP Behrendorff") ~ "Jason Behrendorff",
    str_detect(player_name_vector, "Kane Richardson") ~ "Kane Richardson",
    str_detect(player_name_vector, "LR Morris") ~ "Lance Morris",
    str_detect(player_name_vector, "AF Milne") ~ "Adam Milne",
    str_detect(player_name_vector, "Ben Dwarshuis") ~ "Ben Dwarshuis",
    str_detect(player_name_vector, "PM Siddle") ~ "Peter Siddle",
    str_detect(player_name_vector, "Sean Abbott") ~ "Sean Abbott",
    str_detect(player_name_vector, "T Murphy") ~ "Todd Murphy",
    str_detect(player_name_vector, "TK Curran") ~ "Tom Curran",
    str_detect(player_name_vector, "U Mir") ~ "Usama Mir",
    TRUE ~ player_name_vector
  )
  return(player_name_vector)
}

# Combine into a df
player_wickets <- 
  bind_rows(list_of_player_wickets) |>
  mutate(player = fix_player_names(player)) |>
  rename(market = market_name, player_name = player, player_team = team)

# Output as a csv
write_csv(player_wickets, "Data/T20s/Big Bash/scraped_odds/bet365_player_wickets.csv")