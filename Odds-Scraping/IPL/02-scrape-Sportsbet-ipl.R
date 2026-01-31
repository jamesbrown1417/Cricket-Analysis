# IPL (Indian Premier League) Sportsbet Scraper
# ==============================================
# Uses the centralized scrape_sportsbet function from Functions/

source("Functions/scrape_sportsbet.R")

# IPL-specific team name fixes
fix_team_names_sportsbet_ipl <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Melbourne R") ~ "Melbourne Renegades",
    TRUE ~ team_name_vector
  )
}

# IPL-specific player name fixes
fix_player_names_ipl <- function(player_name_vector) {
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
  sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/indian-premier-league",
  output_dir = "Data/T20s/IPL/scraped_odds",
  competition_name = "IPL",
  fix_team_names = fix_team_names_sportsbet_ipl,
  fix_player_names = fix_player_names_ipl,
  use_live_html = TRUE,
  scrape_milestones = TRUE
)
