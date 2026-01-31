# Internationals (International T20) Sportsbet Scraper
# =====================================================
# Uses the centralized scrape_sportsbet function from Functions/

source("Functions/scrape_sportsbet.R")

# Internationals-specific team name fixes
fix_team_names_sportsbet_internationals <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "UAE|Uae") ~ "United Arab Emirates",
    str_detect(team_name_vector, "Usa") ~ "USA",
    TRUE ~ team_name_vector
  )
}

# Run the scraper
scrape_sportsbet(
  sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/international-twenty20-matches",
  output_dir = "Data/T20s/Internationals/scraped_odds",
  competition_name = "Internationals",
  fix_team_names = fix_team_names_sportsbet_internationals,
  use_live_html = FALSE
)
