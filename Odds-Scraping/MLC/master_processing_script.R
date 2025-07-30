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
run_scraping("Odds-Scraping/MLC/02-scrape-Sportsbet-mlc.R")
run_scraping("Odds-Scraping/MLC/01-scrape-TAB-mlc.R")
# run_scraping("Odds-Scraping/MLC/03-scrape-pointsbet-mlc.R")

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <-
  map(list_of_h2h_files, read_csv)

# Combine
h2h_data <-
  list_of_h2h_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match)

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

# Combine
best_h2h <-
  best_home_win |> 
  inner_join(best_away_win) |> 
  mutate(margin = 100*(round(1/home_win + 1/away_win, 3) - 1)) |> 
  arrange(margin)

#===============================================================================
# Runs at fall of first wicket
#===============================================================================

# Read in all FOFW data
list_of_fofw_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "runs_at_first_wicket.csv")

# Read in all FOFW data
list_of_fofw_data <- map(list_of_fofw_files, read_csv)

# Combine
fofw_data <-
  list_of_fofw_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

#===============================================================================
# First Over Runs
#===============================================================================

# Read in all first over runs data
list_of_for_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "first_over_runs.csv")

# Read in all first over runs data
list_of_for_data <- map(list_of_for_files, read_csv)

# Combine
for_data <-
  list_of_for_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

#===============================================================================
# Match Sixes
#===============================================================================

# Read in all match sixes data
list_of_ms_files <-
  list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "match_sixes") |> 
  keep(~!str_detect(.x, "most"))
  

# Read in all match sixes data
list_of_ms_data <- map(list_of_ms_files, read_csv)

# Combine
ms_data <-
  list_of_ms_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match)

#===============================================================================
# Match Fours
#===============================================================================

# Read in all match fours data
list_of_mf_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "match_fours")

# Read in all match fours data
list_of_mf_data <- map(list_of_mf_files, read_csv)

# Combine
mf_data <-
  list_of_mf_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match)

#===============================================================================
# Team Sixes
#===============================================================================

# Read in all team sixes data
list_of_ts_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "team_sixes")

# Read in all team sixes data
list_of_ts_data <- map(list_of_ts_files, read_csv)

# Combine
ts_data <-
  list_of_ts_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

#===============================================================================
# Team Fours
#===============================================================================

# Read in all team fours data
list_of_tf_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "team_fours")

# Read in all team fours data
list_of_tf_data <- map(list_of_tf_files, read_csv)

# Combine
tf_data <-
  list_of_tf_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team)

#===============================================================================
# Batter Runs
#===============================================================================

# Read in all batter runs data
list_of_br_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "player_runs")

# Read in all batter runs data
list_of_br_data <-
  map(list_of_br_files, read_csv)

# Combine
br_data <-
  list_of_br_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_name)

#===============================================================================
# Highest Opening Partnership
#===============================================================================

# Read in all highest opening partnership data
list_of_hop_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "highest_opening_partnership")

# Read in all highest opening partnership data
list_of_hop_data <- map(list_of_hop_files, read_csv)

# Combine
hop_data <-
  list_of_hop_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  mutate(team = ifelse(team == "Draw", "Tie", team)) |> 
  arrange(match) |> 
  arrange(match, team, desc(price))

#===============================================================================
# Most Match Sixes
#===============================================================================

# Read in all most match sixes data
list_of_mms_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "most.*sixes")

# Read in all most match sixes data
list_of_mms_data <- map(list_of_mms_files, read_csv)

# Combine
mms_data <-
  list_of_mms_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  mutate(team = ifelse(team == "Draw", "Tie", team)) |> 
  arrange(match, team, desc(price))

