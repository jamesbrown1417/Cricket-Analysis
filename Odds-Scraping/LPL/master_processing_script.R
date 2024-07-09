#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <- map_dfr(list_of_h2h_files, read_csv)

# Best Home Win
best_home_win <-
  list_of_h2h_data |> 
  group_by(match) |> 
  arrange(match, desc(home_win)) |> 
  slice_head(n = 1) |> 
  select(-away_win, -margin) |> 
  rename(home_agency = agency)

# Best Away Win
best_away_win <-
  list_of_h2h_data |> 
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
list_of_fofw_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "runs_at_first_wicket")

# Read in all FOFW data
list_of_fofw_data <- map(list_of_fofw_files, read_csv)

#===============================================================================
# First Over Runs
#===============================================================================

# Read in all first over runs data
list_of_for_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "first_over_runs")

# Read in all first over runs data
list_of_for_data <- map(list_of_for_files, read_csv)

#===============================================================================
# Match Sixes
#===============================================================================

# Read in all match sixes data
list_of_ms_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "match_sixes")

# Read in all match sixes data
list_of_ms_data <- map(list_of_ms_files, read_csv)

#===============================================================================
# Match Fours
#===============================================================================

# Read in all match fours data
list_of_mf_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "match_fours")

# Read in all match fours data
list_of_mf_data <- map(list_of_mf_files, read_csv)

#===============================================================================
# Team Sixes
#===============================================================================

# Read in all team sixes data
list_of_ts_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "team_sixes")

# Read in all team sixes data
list_of_ts_data <- map(list_of_ts_files, read_csv)

#===============================================================================
# Team Fours
#===============================================================================

# Read in all team fours data
list_of_tf_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "team_fours")

# Read in all team fours data
list_of_tf_data <- map(list_of_tf_files, read_csv)

#===============================================================================
# Batter Runs
#===============================================================================

# Read in all batter runs data
list_of_br_files <- list.files("Data/T20s/LPL/scraped_odds/", full.names = TRUE, pattern = "player_runs")

# Read in all batter runs data
list_of_br_data <-
  map(list_of_br_files, read_csv)

# Combine
br_data <-
  list_of_br_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_name)
