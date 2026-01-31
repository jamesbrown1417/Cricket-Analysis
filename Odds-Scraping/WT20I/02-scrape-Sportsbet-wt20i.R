# WT20I (ICC Women's T20 World Cup) Sportsbet Scraper
# =====================================================
# Uses the centralized scrape_sportsbet function from Functions/

source("Functions/scrape_sportsbet.R")

# WT20I-specific team name fixes
fix_team_names_sportsbet_wt20i <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Bangladesh") ~ "Bangladesh",
    str_detect(team_name_vector, "England") ~ "England",
    str_detect(team_name_vector, "India") ~ "India",
    str_detect(team_name_vector, "New-Zealand") ~ "New Zealand",
    str_detect(team_name_vector, "Pakistan") ~ "Pakistan",
    str_detect(team_name_vector, "South-Africa") ~ "South Africa",
    str_detect(team_name_vector, "Sri-Lanka") ~ "Sri Lanka",
    str_detect(team_name_vector, "West-Indies") ~ "West Indies",
    str_detect(team_name_vector, "Australia") ~ "Australia",
    TRUE ~ team_name_vector
  )
}

# Run the scraper
scrape_sportsbet(
  sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/icc-womens-t20-world-cup",
  output_dir = "Data/T20s/WT20I/scraped_odds",
  competition_name = "WT20I",
  fix_team_names = fix_team_names_sportsbet_wt20i,
  use_live_html = FALSE
)
