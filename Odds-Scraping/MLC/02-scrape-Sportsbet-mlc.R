# MLC (Major League Cricket) Sportsbet Scraper
# ==============================================
# Uses the centralized scrape_sportsbet function from Functions/

source("Functions/scrape_sportsbet.R")
source("Functions/fix_team_names.R")

# MLC-specific player name fixes
fix_player_names_mlc <- function(player_name_vector) {
  case_when(
    player_name_vector == "Gerhard Erasmus" ~ "Merwe Erasmus",
    player_name_vector == "G Erasmus" ~ "M Erasmus",
    player_name_vector == "D De Silva" ~ "D de Silva",
    player_name_vector == "S Ssesazi" ~ "S Sesazi",
    player_name_vector == "Will Jacks" ~ "William Jacks",
    player_name_vector == "Phil Salt" ~ "Philip Salt",
    player_name_vector == "Jos Buttler" ~ "Joseph Buttler",
    player_name_vector == "George Munsey" ~ "Henry Munsey",
    player_name_vector == "Max ODowd" ~ "Maxwell O'Dowd",
    player_name_vector == "S Smrwickrma" ~ "Wedagedara Samarawickrama",
    player_name_vector == "Jonny Bairstow" ~ "Jonathan Bairstow",
    player_name_vector == "Ollie Hairs" ~ "Oliver Hairs",
    player_name_vector == "Richie Berrington" ~ "Richard Berrington",
    player_name_vector == "JJ Smit" ~ "Johannes Smit",
    player_name_vector == "Jan Nicolaas Frylinck" ~ "Jan Frylinck",
    player_name_vector == "Matty Cross" ~ "Matthew Cross",
    player_name_vector == "Niko Davin" ~ "Nikolaas Davin",
    .default = player_name_vector
  )
}

# Run the scraper
scrape_sportsbet(
  sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/major-league-cricket",
  output_dir = "Data/T20s/Major League Cricket/scraped_odds",
  competition_name = "MLC",
  fix_team_names = fix_team_names_mlc,
  fix_player_names = fix_player_names_mlc,
  use_live_html = TRUE,
  scrape_extra_match_markets = TRUE
)
