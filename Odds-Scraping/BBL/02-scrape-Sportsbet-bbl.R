# BBL (Big Bash League) Sportsbet Scraper
# ========================================
# Uses the centralized scrape_sportsbet function from Functions/

source("Functions/scrape_sportsbet.R")

# BBL-specific team name fixes
fix_team_names_sportsbet_bbl <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Melbourne R") ~ "Melbourne Renegades",
    TRUE ~ team_name_vector
  )
}

# BBL-specific player name fixes
fix_player_names_bbl <- function(player_name_vector) {
  case_when(
    str_detect(player_name_vector, "T.?.? Rogers ?\\(Stars\\)") ~ "Tom Rogers",
    player_name_vector == "Tom Rogers (Stars)" ~ "Tom F Rogers",
    player_name_vector == "Tom Rogers." ~ "Tom Rogers",
    str_detect(player_name_vector, "K Jennings") ~ "Keaton Jennings",
    player_name_vector == "Matthew Kuhnemann" ~ "Matt Kuhnemann",
    player_name_vector == "Mujeeb Ur-Rahman" ~ "Mujeeb Ur Rahman",
    player_name_vector == "Stephen O'Keefe" ~ "Steve O'Keefe",
    player_name_vector == "Matt Renshaw" ~ "Matthew Renshaw",
    player_name_vector == "Oliver Davies" ~ "Ollie Davies",
    .default = player_name_vector
  )
}

# Run the scraper
scrape_sportsbet(
  sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/twenty20-big-bash",
  output_dir = "Data/T20s/Big Bash/scraped_odds",
  competition_name = "BBL",
  fix_team_names = fix_team_names_sportsbet_bbl,
  fix_player_names = fix_player_names_bbl,
  use_live_html = TRUE
)
