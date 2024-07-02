#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(cricketdata)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

`%notin%` <- Negate(`%in%`)

# Update player metadata table
player_meta <- read_rds("Data/player_meta_updated.rds")

# For big bash, make Josh Brown J Brown
player_meta$unique_name[player_meta$full_name == "Josh Brown"] <- "J Brown"

# Cricsheet player ID data
player_metadata <-
  player_meta |>
  select(unique_name,
         full_name,
         country,
         dob,
         batting_style,
         bowling_style,
         playing_role)

#===============================================================================
# Get Data from cricsheet
#===============================================================================

# Match Data--------------------------------------------------------------------
bbl_match_data <-
  fetch_cricsheet(type = "match",
                  competition = "bbl",
                  gender = "male")

# Player Data-------------------------------------------------------------------
bbl_player_data <-
  fetch_cricsheet(type = "player",
                  competition = "bbl",
                  gender = "male")

# Ball by Ball Data-------------------------------------------------------------
bbl_ball_by_ball_data <-
  fetch_cricsheet(type = "bbb",
                  competition = "bbl",
                  gender = "male")

#===============================================================================
# Add player metadata to player data
#===============================================================================

# Add player metadata to player data
bbl_player_data <-
  bbl_player_data |>
  left_join(player_metadata,
            by = c("player" = "unique_name"))


#===============================================================================
# Get Match Innings Data
#===============================================================================

# Create function to get match innings data-------------------------------------
get_match_innings_data <- function(match_id_number) {
  # Match dataset
  match_df <- bbl_ball_by_ball_data |> filter(match_id == match_id_number)
  
  # Match Details
  match_details <- bbl_match_data |> mutate(match_id = as.integer(match_id)) |>  filter(match_id == match_id_number)
  
  # Match Event
  match_event <- match_details$event[[1]]
  
  # Toss Winner
  toss_winner <- match_details$toss_winner[[1]]
  
  # Toss Decision
  toss_decision <- match_details$toss_decision[[1]]
  
  # POTM
  potm <- match_details$player_of_match[[1]]
  
  # Match Date
  match_date <- match_details$date[[1]] |> as_date()
  
  # Match Venue
  match_venue <- match_details$venue[[1]]
  
  # Get First Innings Dataset
  innings_1 <- match_df |> filter(innings == 1)
  
  # Get runs scored, batting team and wickets lost in first innings
  innings_1_data <-
    innings_1 |>
    summarise(
      innings_1_batting_team = first(batting_team),
      innings_1_total = first(innings1_total),
      innings_1_balls = n(),
      innings_1_wickets = sum(wicket),
      innings_1_fours = sum(runs_off_bat == 4),
      innings_1_sixes = sum(runs_off_bat == 6),
      innings_1_extras = sum(extras)
    )
  
  # Get Second Innings Dataset
  innings_2 <- match_df |> filter(innings == 2)
  
  # Get runs scored, batting team and wickets lost in second innings
  innings_2_data <-
    innings_2 |>
    summarise(
      innings_2_batting_team = first(batting_team),
      innings_2_total = first(innings2_total),
      innings_2_balls = n(),
      innings_2_wickets = sum(wicket),
      innings_2_fours = sum(runs_off_bat == 4),
      innings_2_sixes = sum(runs_off_bat == 6),
      innings_2_extras = sum(extras)
    )
  
  # Create Tibble
  tibble(match_id = match_id_number,
         match_date = match_date,
         event = match_event,
         toss_winner = toss_winner,
         toss_decision = toss_decision,
         player_of_the_match = potm,
         venue = match_venue) |> 
    bind_cols(innings_1_data) |>
    bind_cols(innings_2_data)
}

# Get Match Innings Data for all matches----------------------------------------
bbl_match_innings_data <-
  bbl_match_data |> 
  distinct(match_id) |> 
  rename(match_id_number = match_id) |>
  mutate(match_id_number = as.integer(match_id_number)) |>
  pmap_dfr(get_match_innings_data, .progress = TRUE)


#===============================================================================
# Get First Over Data
#===============================================================================

# Create function to get match first_over data-------------------------------------
get_match_first_over_data <- function(match_id_number) {
  # Match dataset
  match_df <- bbl_ball_by_ball_data |> filter(match_id == match_id_number)
  
  # Match Details
  match_details <- bbl_match_data |> mutate(match_id = as.integer(match_id)) |>  filter(match_id == match_id_number)
  
  # Match Date
  match_date <- match_details$date[[1]] |> as_date()
  
  # Match Event
  match_event <- match_details$event[[1]]
  
  # Match Venue
  match_venue <- match_details$venue[[1]]
  
  # Get First Innings First Over Dataset
  first_over_innings_1 <-
    match_df |>
    filter(innings == 1 & over == 1) |> 
    filter(max(ball, na.rm = TRUE) >= 6)
  
  # Get runs scored, batting team and wickets lost in first first_over
  first_over_innings_1_data <-
    first_over_innings_1 |>
    summarise(
      first_over_batting_team = first(batting_team),
      first_over_bowling_team = first(bowling_team),
      first_over_bowler = first(bowler),
      first_over_total = last(runs_scored_yet),
      first_over_balls = n(),
      first_over_wickets = sum(wicket),
      first_over_fours = sum(runs_off_bat == 4),
      first_over_sixes = sum(runs_off_bat == 6),
      first_over_extras = sum(extras),
      innings = first(innings)
    )
  
  # Get Second Innings First Over Dataset
  first_over_innings_2 <-
    match_df |>
    filter(innings == 2 & over == 1) |> 
    filter(max(ball, na.rm = TRUE) >= 6)
  
  # Get runs scored, batting team and wickets lost in first first_over
  first_over_innings_2_data <-
    first_over_innings_2 |>
    summarise(
      first_over_batting_team = first(batting_team),
      first_over_bowling_team = first(bowling_team),
      first_over_bowler = first(bowler),
      first_over_total = last(runs_scored_yet),
      first_over_balls = n(),
      first_over_wickets = sum(wicket),
      first_over_fours = sum(runs_off_bat == 4),
      first_over_sixes = sum(runs_off_bat == 6),
      first_over_extras = sum(extras),
      innings = first(innings)
    )
  
  # Create Tibble
  first_over_innings_1_data |> 
    bind_rows(first_over_innings_2_data) |> 
    mutate(match_id = match_id_number,
           match_date = match_date,
           event = match_event,
           venue = match_venue) |> 
    relocate(match_id, match_date, event, venue, .before = first_over_batting_team) |> 
    relocate(innings, .after = venue)
}

# Get Match first_over Data for all matches----------------------------------------
bbl_match_first_over_data <-
  bbl_match_data |> 
  distinct(match_id) |> 
  rename(match_id_number = match_id) |>
  mutate(match_id_number = as.integer(match_id_number)) |>
  pmap_dfr(get_match_first_over_data, .progress = TRUE) |> 
  filter(!is.na(first_over_total))

#===============================================================================
# Get Batter Match by Match Data
#===============================================================================

# Create function to batting data using match ID and player unique name---------
get_player_match_data <- function(match_id_number, player_name) {
  # Player dataset
  player_df <-
    bbl_ball_by_ball_data |>
    filter(match_id == match_id_number) |>
    filter(striker == player_name | non_striker == player_name)
  
  # Player Team
  player_team <- player_df$batting_team[[1]]
  
  # Innings
  innings_num <- player_df$innings[[1]]
  
  # Balls Faced
  balls_faced <- player_df |> filter(striker == player_name) |> nrow()
  
  # Runs Scored
  runs_scored <- player_df |> filter(striker == player_name) |> summarise(runs = sum(runs_off_bat)) |> pull()
  
  # Fours
  fours <- player_df |> filter(striker == player_name) |> summarise(fours = sum(runs_off_bat == 4)) |> pull()
  
  # Sixes
  sixes <- player_df |> filter(striker == player_name) |> summarise(sixes = sum(runs_off_bat == 6)) |> pull()
  
  # Dismissal
  dismissal <- player_df |> 
    filter(player_dismissed == player_name) |> 
    summarise(dismissal_method = if_else(n() == 0, "Not Out", first(wicket_type))) %>%
    pull(dismissal_method)
  
  # Create Tibble
  tibble(match_id = match_id_number,
         player = player_name,
         player_team = player_team,
         innings = innings_num,
         balls_faced = balls_faced,
         runs_scored = runs_scored,
         fours = fours,
         sixes = sixes,
         dismissal = dismissal) |> 
    mutate(strike_rate = runs_scored / balls_faced * 100)
}

# Get safe version of function
get_player_match_data_safe <- safely(get_player_match_data)

# Get tibble of match_id_number and player_name to iterate over-----------------
bbl_batting_innings_level <-
  bbl_ball_by_ball_data |>
  pivot_longer(cols = c(striker, non_striker), names_to = "role", values_to = "batter") |> 
  select(match_id_number = match_id, player_name = batter) |> 
  distinct() |> 
  future_pmap(get_player_match_data_safe, .progress = TRUE)

# Remove errors and bind rows
bbl_batting_innings_level <-
  bbl_batting_innings_level |>
  # Keep result part
  map_dfr("result")

# Join match details
bbl_batting_innings_level <-
  bbl_batting_innings_level |>
  left_join(bbl_match_data |> mutate(match_id = as.integer(match_id)),
            by = "match_id")

# Create function to get batting order------------------------------------------
get_batting_order <- function(match_id_number, innings_num) {
  
  # Get batting order 
  bbl_ball_by_ball_data |>
    filter(match_id == match_id_number) |>
    filter(innings == innings_num) |>
    mutate(ball = 6*(over-1) + ball) |> 
    select(ball, striker, non_striker) |> 
    pivot_longer(cols = c(striker, non_striker), names_to = "role", values_to = "batter") |> 
    distinct(batter, .keep_all = TRUE) |>
    arrange(ball) |>
    mutate(batting_position = row_number()) |>
    select(batting_position, batter) |> 
    mutate(match_id = match_id_number,
           innings = innings_num)
}

# Get safe version of function
get_batting_order_safe <- safely(get_batting_order)

# Get tibble of match_id_number and innings_num to iterate over-----------------
bbl_batting_order <-
  bbl_ball_by_ball_data |>
  select(match_id_number = match_id, innings_num = innings) |> 
  distinct() |> 
  future_pmap(get_batting_order_safe, .progress = TRUE) |> 
  map_dfr("result")

# Add to batting innings level data
bbl_batting_innings_level <-
  bbl_batting_innings_level |>
  left_join(bbl_batting_order,
            by = c("match_id" = "match_id", "innings" = "innings", "player" = "batter"))

# Add player metadata to player data
bbl_batting_innings_level <-
  bbl_batting_innings_level |>
  left_join(player_metadata,
            by = c("player" = "unique_name"))

#===============================================================================
# Get Bowler Match by Match Data
#===============================================================================

# Create function to batting data using match ID and player unique name---------
get_bowler_match_data <- function(match_id_number, player_name) {
  # Player dataset
  player_df <-
    bbl_ball_by_ball_data |>
    filter(match_id == match_id_number) |>
    filter(bowler == player_name)
  
  # Player Team
  player_team <- player_df$bowling_team[[1]]
  
  # Innings
  innings_num <- player_df$innings[[1]]
  
  # Balls Bowled
  balls_bowled <- player_df |> nrow()
  
  # Runs Conceded
  runs_conceded <- player_df |> summarise(runs = sum(runs_off_bat, na.rm = TRUE) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE)) |> pull()
  
  # Fours
  fours_conceded <- player_df |> summarise(fours = sum(runs_off_bat == 4)) |> pull()
  
  # Sixes
  sixes_conceded <- player_df |> summarise(sixes = sum(runs_off_bat == 6)) |> pull()
  
  # Wickets
  wickets <-
    player_df |> 
    filter(wicket) |> 
    filter(wicket_type != "run out") |> 
    nrow()
  
  # Create Tibble
  tibble(match_id = match_id_number,
         player = player_name,
         player_team = player_team,
         innings = innings_num,
         balls_bowled = balls_bowled,
         runs_conceded = runs_conceded,
         fours_conceded = fours_conceded,
         sixes_conceded = sixes_conceded,
         wickets = wickets) |>
    mutate(economy_rate = runs_conceded / balls_bowled * 6)
}

# Get safe version of function
get_bowler_match_data_safe <- safely(get_bowler_match_data)

# Get tibble of match_id_number and player_name to iterate over-----------------
bbl_bowling_innings_level <-
  bbl_ball_by_ball_data |>
  select(match_id_number = match_id, player_name = bowler) |> 
  distinct() |> 
  future_pmap(get_bowler_match_data_safe, .progress = TRUE) |> 
  map_dfr("result")

# Join match details
bbl_bowling_innings_level <-
  bbl_bowling_innings_level |>
  left_join(bbl_match_data |> mutate(match_id = as.integer(match_id)),
            by = "match_id")

# Add player metadata to player data
bbl_bowling_innings_level <-
  bbl_bowling_innings_level |>
  left_join(player_metadata,
            by = c("player" = "unique_name"))

#===============================================================================
# Output Innings Data
#===============================================================================

# Output Data
write_rds(bbl_match_innings_data, "Data/T20s/Big Bash/bbl_match_innings_data.rds")

#===============================================================================
# Output First Over Data
#===============================================================================

# Output Data
write_rds(bbl_match_first_over_data, "Data/T20s/Big Bash/bbl_first_over_data.rds")

#===============================================================================
# Tidy and output batting data
#===============================================================================

# Tidy batting data
bbl_batting_innings_level <-
  bbl_batting_innings_level |>
  transmute(
    match_id,
    team1,
    team2,
    match_date = as_date(date),
    venue,
    city,
    event,
    match_number,
    toss_winner,
    toss_decision,
    winner = ifelse(!is.na(winner), winner, outcome),
    method,
    winner_runs,
    winner_wickets,
    innings,
    player_unique_name = player,
    player_full_name = full_name,
    player_team,
    batting_style,
    playing_role,
    player_dob = dob,
    batting_position,
    runs_scored,
    balls_faced,
    dismissal,
    fours,
    sixes,
    strike_rate)

# Output Data
write_rds(bbl_batting_innings_level, "Data/T20s/Big Bash/bbl_batting_innings_level.rds")

#===============================================================================
# Tidy and output bowling data
#===============================================================================

# Tidy bowling data
bbl_bowling_innings_level <-
  bbl_bowling_innings_level |>
  transmute(
    match_id,
    team1,
    team2,
    match_date = as_date(date),
    venue,
    city,
    event,
    match_number,
    toss_winner,
    toss_decision,
    winner = ifelse(!is.na(winner), winner, outcome),
    method,
    winner_runs,
    winner_wickets,
    innings,
    player_unique_name = player,
    player_full_name = full_name,
    player_team,
    bowling_style,
    playing_role,
    player_dob = dob,
    balls_bowled,
    runs_conceded,
    fours_conceded,
    sixes_conceded,
    wickets,
    economy_rate)

# Output Data
write_rds(bbl_bowling_innings_level, "Data/T20s/Big Bash/bbl_bowling_innings_level.rds")