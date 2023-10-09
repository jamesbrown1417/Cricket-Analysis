##%######################################################%##
#                                                          #
####           Get historical test match data           ####
#                                                          #
##%######################################################%##

# Libraries
library(tidyverse)
library(cricketdata)

##%######################################################%##
#                                                          #
####              Test batsmen career data              ####
#                                                          #
##%######################################################%##

# Get all Data
test_batsmen_career <- cricketdata::fetch_cricinfo(matchtype = "test",activity = "batting", type = "career")

# Get only those with above 25 career innings
test_batsmen_career_25_plus_matches <- test_batsmen_career %>% filter(Matches >= 25)

# Fix Country column
test_batsmen_career_25_plus_matches$Country <- str_remove_all(test_batsmen_career_25_plus_matches$Country, "(/)|(ICC)|( World XI)")
test_batsmen_career_25_plus_matches$Country <- str_replace_all(test_batsmen_career_25_plus_matches$Country, "BAN", "Bangladesh")
test_batsmen_career_25_plus_matches$Country <- str_replace_all(test_batsmen_career_25_plus_matches$Country, "AustraliaEngland", "England")
test_batsmen_career_25_plus_matches$Country <- str_replace_all(test_batsmen_career_25_plus_matches$Country, "AustraliaSA", "South Africa")
test_batsmen_career_25_plus_matches$Country <- str_replace_all(test_batsmen_career_25_plus_matches$Country, "IndiaPakistan", "Pakistan")
test_batsmen_career_25_plus_matches$Country <- str_replace_all(test_batsmen_career_25_plus_matches$Country, "SA", "South Africa")

# Get rid of leading and trailing whitespace
test_batsmen_career_25_plus_matches$Country <- trimws(test_batsmen_career_25_plus_matches$Country)
test_batsmen_career_25_plus_matches$Player <- trimws(test_batsmen_career_25_plus_matches$Player)

##%######################################################%##
#                                                          #
####              Test bowlers career data              ####
#                                                          #
##%######################################################%##

# Get all Data
test_bowlers_career <- cricketdata::fetch_cricinfo(matchtype = "test",activity = "bowling", type = "career")

# Get only those with above 25 career innings
test_bowlers_career_25_plus_matches <- test_bowlers_career %>% filter(Matches >= 25) 

# Remove other teams in Player column
test_bowlers_career_25_plus_matches$Player <- str_remove_all(test_bowlers_career_25_plus_matches$Player, " \\(.*$")

# Get rid of leading and trailing whitespace
test_bowlers_career_25_plus_matches$Country <- trimws(test_bowlers_career_25_plus_matches$Country)
test_bowlers_career_25_plus_matches$Player <- trimws(test_bowlers_career_25_plus_matches$Player)

# Fix Country column
test_bowlers_career_25_plus_matches <-
  test_bowlers_career_25_plus_matches %>%
  left_join(test_batsmen_career_25_plus_matches[,c("Player", "Country")], by = "Player") %>%
  mutate(Country.x = ifelse(is.na(Country.x), Country.y, Country.x)) %>%
  select(-Country.y) %>%
  rename(Country = Country.x) %>%
  relocate(Country, .after = Player)

##%######################################################%##
#                                                          #
####                   Write out Data                   ####
#                                                          #
##%######################################################%##

write_rds(test_bowlers_career_25_plus_matches, "Data/Tests/test-career-stats-bowlers.RDS")
write_rds(test_batsmen_career_25_plus_matches, "Data/Tests/test-career-stats-batsmen.RDS")
