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

# Get only those with above 20 career innings
test_batsmen_career_20_plus_innings <- test_batsmen_career %>% filter(Innings >= 20)

# Fix Country column
test_batsmen_career_20_plus_innings$Country <- str_remove_all(test_batsmen_career_20_plus_innings$Country, "(/)|(ICC)|( World XI)")
test_batsmen_career_20_plus_innings$Country <- str_replace_all(test_batsmen_career_20_plus_innings$Country, "BAN", "Bangladesh")
test_batsmen_career_20_plus_innings$Country <- str_replace_all(test_batsmen_career_20_plus_innings$Country, "AustraliaEngland", "England")
test_batsmen_career_20_plus_innings$Country <- str_replace_all(test_batsmen_career_20_plus_innings$Country, "AustraliaSA", "South Africa")
test_batsmen_career_20_plus_innings$Country <- str_replace_all(test_batsmen_career_20_plus_innings$Country, "IndiaPakistan", "Pakistan")
test_batsmen_career_20_plus_innings$Country <- str_replace_all(test_batsmen_career_20_plus_innings$Country, "SA", "South Africa")

# Get rid of leading and trailing whitespace
test_batsmen_career_20_plus_innings$Country <- trimws(test_batsmen_career_20_plus_innings$Country)
test_batsmen_career_20_plus_innings$Player <- trimws(test_batsmen_career_20_plus_innings$Player)

##%######################################################%##
#                                                          #
####              Test bowlers career data              ####
#                                                          #
##%######################################################%##

# Get all Data
test_bowlers_career <- cricketdata::fetch_cricinfo(matchtype = "test",activity = "bowling", type = "career")

# Get only those with above 20 career innings
test_bowlers_career_20_plus_innings <- test_bowlers_career %>% filter(Innings >= 20) 

# Remove other teams in Player column
test_bowlers_career_20_plus_innings$Player <- str_remove_all(test_bowlers_career_20_plus_innings$Player, " \\(.*$")

# Get rid of leading and trailing whitespace
test_bowlers_career_20_plus_innings$Country <- trimws(test_bowlers_career_20_plus_innings$Country)
test_bowlers_career_20_plus_innings$Player <- trimws(test_bowlers_career_20_plus_innings$Player)

# Fix Country column
test_bowlers_career_20_plus_innings <-
  test_bowlers_career_20_plus_innings %>%
  left_join(test_batsmen_career_20_plus_innings[,c("Player", "Country")], by = "Player") %>%
  mutate(Country.x = ifelse(is.na(Country.x), Country.y, Country.x)) %>%
  select(-Country.y) %>%
  rename(Country = Country.x) %>%
  relocate(Country, .after = Player)

##%######################################################%##
#                                                          #
####                   Write out Data                   ####
#                                                          #
##%######################################################%##

write_rds(test_bowlers_career_20_plus_innings, "Data/Tests/test-career-stats-bowlers.RDS")
write_rds(test_batsmen_career_20_plus_innings, "Data/Tests/test-career-stats-batsmen.RDS")
