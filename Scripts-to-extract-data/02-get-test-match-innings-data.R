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
####             Test batsmen innings data              ####
#                                                          #
##%######################################################%##

# Get all Data
test_batsmen_innings <- cricketdata::fetch_cricinfo(matchtype = "test",activity = "batting", type = "innings")

# Add match ID variable
test_batsmen_innings <-
  test_batsmen_innings %>%
  arrange(Date) %>%
  group_by(Date, Ground) %>%
  mutate(TestNumber = cur_group_id()) %>%
  relocate(TestNumber, .after = Date)

# Get rid of leading and trailing whitespace
test_batsmen_innings$Country <- trimws(test_batsmen_innings$Country)
test_batsmen_innings$Player <- trimws(test_batsmen_innings$Player)
test_batsmen_innings$Ground <- trimws(test_batsmen_innings$Ground)
test_batsmen_innings$Opposition <- trimws(test_batsmen_innings$Opposition)

# Save as RDS
write_rds(test_batsmen_innings, "Data/Tests/test-innings-stats-batsmen.RDS")
