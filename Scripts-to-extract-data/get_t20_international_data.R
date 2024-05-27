#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(cricketdata)
`%notin%` <- Negate(`%in%`)

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
t20_international_match_data <-
  fetch_cricsheet(type = "match",
                  competition = "t20s",
                  gender = "male")

# Player Data-------------------------------------------------------------------
t20_international_player_data <-
  fetch_cricsheet(type = "player",
                  competition = "t20s",
                  gender = "male")

# Ball by Ball Data-------------------------------------------------------------
t20_international_ball_by_ball_data <-
  fetch_cricsheet(type = "bbb",
                  competition = "t20s",
                  gender = "male")

#===============================================================================
# Add player metadata to player data
#===============================================================================

# Add player metadata to player data
t20_international_player_data <-
  t20_international_player_data |>
  left_join(player_metadata,
            by = c("player" = "unique_name"))
