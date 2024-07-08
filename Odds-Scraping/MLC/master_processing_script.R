#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <- map_dfr(list_of_h2h_files, read_csv)

# Best Home Win
best_home_win <-
  list_of_h2h_data |> 
  arrange(match, desc(home_win)) |> 
  group_by(match) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(-away_win, -margin) |> 
  rename(home_agency = agency)

# Best Away Win
best_away_win <-
  list_of_h2h_data |> 
  arrange(match, desc(away_win)) |>
  group_by(match) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(-home_win, -margin) |>
  rename(away_agency = agency)

# Combine
best_h2h <-
best_home_win |> 
  inner_join(best_away_win) |> 
  mutate(margin = 100*(round(1/home_win + 1/away_win, 3) - 1))

#===============================================================================
# Runs at fall of first wicket
#===============================================================================

# Read in all FOFW data
list_of_fofw_files <- list.files("Data/T20s/Major League Cricket/scraped_odds/", full.names = TRUE, pattern = "runs_at_first_wicket")

# Read in all FOFW data
list_of_fofw_data <- map(list_of_fofw_files, read_csv)
