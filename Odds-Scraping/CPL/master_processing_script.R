#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name, echo = FALSE)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
# run_scraping("Odds-Scraping/CPL/scrape_betr.R")
# run_scraping("Odds-Scraping/CPL/scrape_BetRight.R")
# run_scraping("Odds-Scraping/CPL/scrape_pointsbet.R")
run_scraping("Odds-Scraping/CPL/02-scrape-Sportsbet.R")
run_scraping("Odds-Scraping/CPL/01-scrape-TAB.R")
run_scraping("Odds-Scraping/CPL/04-scrape-topsport.R")
# run_scraping("Odds-Scraping/CPL/scrape_bet365.R")
# run_scraping("Odds-Scraping/CPL/scrape_bluebet.R")
# run_scraping("Odds-Scraping/CPL/Neds/scrape_neds.R")
# run_scraping("Odds-Scraping/CPL/scrape_unibet.R")
# run_scraping("Odds-Scraping/CPL/scrape_dabble.R")

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <-
  map(list_of_h2h_files, read_csv)

# Combine
h2h_data <-
  list_of_h2h_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(-start_time)

# Best Home Win
best_home_win <-
  h2h_data |> 
  group_by(match) |> 
  arrange(match, desc(home_win)) |> 
  slice_head(n = 1) |> 
  select(-away_win, -margin) |> 
  rename(home_agency = agency)

# Best Away Win
best_away_win <-
  h2h_data |> 
  group_by(match) |>
  arrange(match, desc(away_win)) |>
  slice_head(n = 1) |>
  select(-home_win, -margin) |>
  rename(away_agency = agency)

# Write out 
write_rds(h2h_data, "Data/T20s/CPL/processed_odds/h2h.rds")

#===============================================================================
# Runs at fall of first wicket
#===============================================================================

# Read in all FOFW data
list_of_fofw_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "runs_at_first_wicket.csv")

# Read in all FOFW data
list_of_fofw_data <- map(list_of_fofw_files, read_csv)

# Combine
fofw_data <-
  list_of_fofw_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team) |> 
  select(-home_team, -away_team)

# Write out 
write_rds(fofw_data, "Data/T20s/CPL/processed_odds/runs_at_first_wicket.rds")

#===============================================================================
# First Over Runs
#===============================================================================

# Read in all first over runs data
list_of_for_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "first_over_runs.csv")

# Read in all first over runs data
list_of_for_data <- map(list_of_for_files, read_csv)

# Combine
for_data <-
  list_of_for_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

# Write out 
write_rds(for_data, "Data/T20s/CPL/processed_odds/first_over_runs.rds")

#===============================================================================
# Match Sixes
#===============================================================================

# Read in all match sixes data
list_of_ms_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "match_sixes")

# Read in all match sixes data
list_of_ms_data <- map(list_of_ms_files, read_csv)

# Combine
ms_data <-
  list_of_ms_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match)

# Write out 
write_rds(ms_data, "Data/T20s/CPL/processed_odds/match_sixes.rds")

#===============================================================================
# Match Fours
#===============================================================================

# Read in all match fours data
list_of_mf_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "match_fours")

# Read in all match fours data
list_of_mf_data <- map(list_of_mf_files, read_csv)

# Combine
mf_data <-
  list_of_mf_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match)

# Write out 
write_rds(mf_data, "Data/T20s/CPL/processed_odds/match_fours.rds")

#===============================================================================
# Team Sixes
#===============================================================================

# Read in all team sixes data
list_of_ts_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "team_sixes")

# Read in all team sixes data
list_of_ts_data <- map(list_of_ts_files, read_csv)

# Combine
ts_data <-
  list_of_ts_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

# Write out
write_rds(ts_data, "Data/T20s/CPL/processed_odds/team_sixes.rds")

#===============================================================================
# Team Fours
#===============================================================================

# Read in all team fours data
list_of_tf_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "team_fours")

# Read in all team fours data
list_of_tf_data <- map(list_of_tf_files, read_csv)

# Combine
tf_data <-
  list_of_tf_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

# Write out
write_rds(tf_data, "Data/T20s/CPL/processed_odds/team_fours.rds")

#===============================================================================
# Batter Runs
#===============================================================================

# Read in all batter runs data
list_of_br_files <- list.files("Data/T20s/CPL/scraped_odds/", full.names = TRUE, pattern = "player_runs")

# Read in all batter runs data
list_of_br_data <-
  map(list_of_br_files, read_csv)

# Combine
br_data <-
  list_of_br_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_name) |> 
  select(-home_team, -away_team) |> 
  arrange(match,player_name, line, desc(over_price))

# Write out
write_rds(br_data, "Data/T20s/CPL/processed_odds/player_runs.rds")
