# CPL (Caribbean Premier League) Sportsbet Scraper
# =================================================
# Uses the centralized scrape_sportsbet function from Functions/

source("Functions/scrape_sportsbet.R")

# CPL-specific team name fixes
fix_team_names_sportsbet_cpl <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Antigua") ~ "Antigua and Barbuda Falcons",
    str_detect(team_name_vector, "St Kitts") ~ "St Kitts and Nevis Patriots",
    str_detect(team_name_vector, "Guyana") ~ "Guyana Amazon Warriors",
    str_detect(team_name_vector, "Trinbago") ~ "Trinbago Knight Riders",
    str_detect(team_name_vector, "Barbados") ~ "Barbados Royals",
    str_detect(team_name_vector, "St Lucia") ~ "St Lucia Kings",
    TRUE ~ team_name_vector
  )
}

# Run the scraper
scrape_sportsbet(
  sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/caribbean-premier-league",
  output_dir = "Data/T20s/CPL/scraped_odds",
  competition_name = "CPL",
  fix_team_names = fix_team_names_sportsbet_cpl,
  use_live_html = FALSE
)
