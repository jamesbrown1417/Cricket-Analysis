#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

# Function to Run all odds scraping scripts-------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name, echo = FALSE)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts-------------------------------------------------
# run_scraping("Odds-Scraping/BBL/scrape_betr.R")
run_scraping("Odds-Scraping/IPL/05-scrape-betright-ipl.R")
run_scraping("Odds-Scraping/IPL/03-scrape-pointsbet-ipl.R")
run_scraping("Odds-Scraping/IPL/02-scrape-Sportsbet-ipl.R")
run_scraping("Odds-Scraping/IPL/TAB/01-scrape-TAB-ipl.R")
# run_scraping("Odds-Scraping/IPL/04-scrape-topsport-ipl.R")
# run_scraping("Odds-Scraping/BBL/Neds/scrape_neds.R")
# run_scraping("Odds-Scraping/BBL/scrape_dabble.R")

#===============================================================================
# Read in all H2H
#===============================================================================

# Read in all H2H data
list_of_h2h_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "h2h")

# Read in all H2H data
list_of_h2h_data <-
  map(list_of_h2h_files, read_csv)

# Combine
h2h_data <-
  list_of_h2h_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  mutate(competition = "BBL")

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
write_rds(h2h_data, "Data/T20s/Big Bash/processed_odds/h2h.rds")

#===============================================================================
# Runs at fall of first wicket
#===============================================================================

# Read in all FOFW data
list_of_fofw_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "runs_at_first_wicket.csv")

# Read in all FOFW data
list_of_fofw_data <- map(list_of_fofw_files, read_csv)

# Combine
fofw_data <-
  list_of_fofw_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  mutate(competition = "BBL")

# Write out 
write_rds(fofw_data, "Data/T20s/Big Bash/processed_odds/runs_at_first_wicket.rds")

#===============================================================================
# First Over Runs
#===============================================================================

# Read in all first over runs data
list_of_for_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "first_over_runs.csv")

# Read in all first over runs data
list_of_for_data <- map(list_of_for_files, read_csv)

# Combine
for_data <-
  list_of_for_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team) |> 
  mutate(competition = "BBL")

# Write out 
write_rds(for_data, "Data/T20s/Big Bash/processed_odds/first_over_runs.rds")

#===============================================================================
# Match Sixes
#===============================================================================

# Read in all match sixes data
list_of_ms_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "match_sixes|total_sixes")

# Read in all match sixes data
list_of_ms_data <- map(list_of_ms_files, read_csv)

# Combine
ms_data <-
  list_of_ms_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  mutate(competition = "BBL")

# Write out
write_rds(ms_data, "Data/T20s/Big Bash/processed_odds/match_sixes.rds")

#===============================================================================
# Match Fours
#===============================================================================

# Read in all match fours data
list_of_mf_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "match_fours|match_4s|total_fours")

# Read in all match fours data
list_of_mf_data <- map(list_of_mf_files, read_csv)

# Combine
mf_data <-
  list_of_mf_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match) |> 
  select(-any_of(c("home_team", "away_team"))) |>
  mutate(competition = "BBL")

# Write out 
write_rds(mf_data, "Data/T20s/Big Bash/processed_odds/match_fours.rds")

#===============================================================================
# Team Sixes
#===============================================================================

# Read in all team sixes data
list_of_ts_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "team_sixes|team_total_6s")

# Read in all team sixes data
list_of_ts_data <- map(list_of_ts_files, read_csv)

# Combine
ts_data <-
  list_of_ts_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team) |> 
  select(-any_of(c("home_team", "away_team"))) |>
  mutate(competition = "BBL")

# Write out
write_rds(ts_data, "Data/T20s/Big Bash/processed_odds/team_sixes.rds")

#===============================================================================
# Team Fours
#===============================================================================

# Read in all team fours data
list_of_tf_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "team_fours|team_total_4s")

# Read in all team fours data
list_of_tf_data <- map(list_of_tf_files, read_csv)

# Combine
tf_data <-
  list_of_tf_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, team) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  mutate(competition = "BBL")

# Write out
write_rds(tf_data, "Data/T20s/Big Bash/processed_odds/team_fours.rds")

#===============================================================================
# Batter Runs
#===============================================================================

# Read in all batter runs data
list_of_br_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "player_runs")

# Read in all batter runs data
list_of_br_data <-
  map(list_of_br_files, read_csv)

# Combine
br_data <-
  list_of_br_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_name) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  arrange(match,player_name, line, desc(over_price)) |> 
  mutate(competition = "BBL") |> 
  mutate(market = "Player Runs") |> 
  arrange(match, player_name, line, desc(over_price))

# Write out
write_rds(br_data, "Data/T20s/Big Bash/processed_odds/player_runs.rds")

#===============================================================================
# Batter Boundaries
#===============================================================================

# Read in all batter boundaries data
list_of_bb_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "player_boundaries")

# Read in all batter boundaries data
list_of_bb_data <- map(list_of_bb_files, read_csv)

# Combine
bb_data <-
  list_of_bb_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_name) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  arrange(match,player_name, line, desc(over_price)) |> 
  mutate(competition = "BBL")

# Sixes
player_sixes <-
  bb_data |> 
  filter(str_detect(market, "Sixes|6s|sixes")) |>
  mutate(market = "Player Sixes") %>% 
  select(-any_of(c("player_team")))

# Fours
player_fours <-
  bb_data |> 
  filter(str_detect(market, "Fours|4s|fours")) |>
  mutate(market = "Player Fours") %>% 
  select(-any_of(c("player_team")))

# Write out
write_rds(player_sixes, "Data/T20s/Big Bash/processed_odds/player_sixes.rds")
write_rds(player_fours, "Data/T20s/Big Bash/processed_odds/player_fours.rds")

#===============================================================================
# Bowler Wickets
#===============================================================================

# Read in all bowler wickets data
list_of_bw_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "player_wickets")

# Read in all bowler wickets data
list_of_bw_data <- map(list_of_bw_files, read_csv)

# Combine
bw_data <-
  list_of_bw_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_name) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  arrange(match,player_name, line, desc(over_price)) |> 
  mutate(competition = "BBL")

# Write out
write_rds(bw_data, "Data/T20s/Big Bash/processed_odds/player_wickets.rds")

#===============================================================================
# Top Team Wicket Taker
#===============================================================================

# Read in all top team wicket taker data
list_of_ttwt_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "top_team_wicket_taker")

# Read in all top team wicket taker data
list_of_ttwt_data <- map(list_of_ttwt_files, read_csv)

# Combine
ttwt_data <-
  list_of_ttwt_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |> 
  arrange(match, player_team, player_name, desc(price)) |> 
    select(-any_of(c("home_team", "away_team"))) |>
  filter(!is.na(price)) |> 
  mutate(competition = "BBL")

#===============================================================================
# Highest Opening Partnership
#===============================================================================

# Read in all highest opening partnership data
list_of_hop_files <- list.files("Data/T20s/Big Bash/scraped_odds/", full.names = TRUE, pattern = "highest_opening_partnership")

# Read in all highest opening partnership data
list_of_hop_data <- map(list_of_hop_files, read_csv)

# Combine
hop_data <-
  list_of_hop_data |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |>
  mutate(competition = "BBL")

# Write out
write_rds(hop_data, "Data/T20s/Big Bash/processed_odds/highest_opening_partnership.rds")
