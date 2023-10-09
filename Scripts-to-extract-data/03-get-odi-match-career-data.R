##%######################################################%##
#                                                          #
####           Get historical ODI match data           ####
#                                                          #
##%######################################################%##

# Libraries
library(tidyverse)
library(cricketdata)

##%######################################################%##
#                                                          #
####              ODI batsmen career data              ####
#                                                          #
##%######################################################%##

# Get all Data
ODI_batsmen_career <- cricketdata::fetch_cricinfo(matchtype = "ODI",activity = "batting", type = "career")

# Get only those with above 20 career innings
ODI_batsmen_career_25_plus_matches <- ODI_batsmen_career %>% filter(Matches >= 25)

# Fix Country column
ODI_batsmen_career_25_plus_matches$Country <- str_remove_all(ODI_batsmen_career_25_plus_matches$Country, "(/)|(ICC)|( World XI)")
ODI_batsmen_career_25_plus_matches$Country <- str_replace_all(ODI_batsmen_career_25_plus_matches$Country, "BAN", "Bangladesh")
ODI_batsmen_career_25_plus_matches$Country <- str_replace_all(ODI_batsmen_career_25_plus_matches$Country, "AustraliaEngland", "England")
ODI_batsmen_career_25_plus_matches$Country <- str_replace_all(ODI_batsmen_career_25_plus_matches$Country, "AustraliaSA", "South Africa")
ODI_batsmen_career_25_plus_matches$Country <- str_replace_all(ODI_batsmen_career_25_plus_matches$Country, "IndiaPakistan", "Pakistan")
ODI_batsmen_career_25_plus_matches$Country <- str_replace_all(ODI_batsmen_career_25_plus_matches$Country, "SA", "South Africa")

# Get rid of leading and trailing whitespace
ODI_batsmen_career_25_plus_matches$Country <- trimws(ODI_batsmen_career_25_plus_matches$Country)
ODI_batsmen_career_25_plus_matches$Player <- trimws(ODI_batsmen_career_25_plus_matches$Player)

##%######################################################%##
#                                                          #
####              ODI bowlers career data              ####
#                                                          #
##%######################################################%##

# Get all Data
ODI_bowlers_career <- cricketdata::fetch_cricinfo(matchtype = "ODI",activity = "bowling", type = "career")

# Get only those with above 20 career innings
ODI_bowlers_career_25_plus_matches <- ODI_bowlers_career %>% filter(Matches >= 25) 

# Remove other teams in Player column
ODI_bowlers_career_25_plus_matches$Player <- str_remove_all(ODI_bowlers_career_25_plus_matches$Player, " \\(.*$")

# Get rid of leading and trailing whitespace
ODI_bowlers_career_25_plus_matches$Country <- trimws(ODI_bowlers_career_25_plus_matches$Country)
ODI_bowlers_career_25_plus_matches$Player <- trimws(ODI_bowlers_career_25_plus_matches$Player)

# Fix Country column
ODI_bowlers_career_25_plus_matches <-
  ODI_bowlers_career_25_plus_matches %>%
  left_join(ODI_batsmen_career_25_plus_matches[,c("Player", "Country")], by = "Player") %>%
  mutate(Country.x = ifelse(is.na(Country.x), Country.y, Country.x)) %>%
  select(-Country.y) %>%
  rename(Country = Country.x) %>%
  relocate(Country, .after = Player)

##%######################################################%##
#                                                          #
####                   Write out Data                   ####
#                                                          #
##%######################################################%##

write_rds(ODI_bowlers_career_25_plus_matches, "Data/ODIs/ODI-career-stats-bowlers.RDS")
write_rds(ODI_batsmen_career_25_plus_matches, "Data/ODIs/ODI-career-stats-batsmen.RDS")
