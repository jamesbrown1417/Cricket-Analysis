# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# URL of website
topsport_url = "https://www.topsport.com.au/Sport/Cricket/Caribbean_Twenty20/Matches"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
main_markets <-
  topsport_url |> 
  read_html() |>
  html_nodes(".marketTable") |> 
  html_table()

#===============================================================================
# Use rvest to get additional market information-------------------------------#
#===============================================================================

# Get links to other markets
topsport_other_markets <-
  topsport_url |>
  read_html() |>
  html_nodes("dd") |> 
  html_attr("data-compurl")

# Remove NA
topsport_other_markets <- topsport_other_markets[!is.na(topsport_other_markets)]

# Remove ?issubcomp=true
topsport_other_markets <- str_remove(topsport_other_markets, "\\?issubcomp=true")

# Add base url
topsport_other_markets <- paste0("https://www.topsport.com.au", topsport_other_markets)

# Get only distinct URLs
topsport_other_markets <- unique(topsport_other_markets)

# Function to fix team names for TopSport CPL
fix_team_names <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Antigua") ~ "Antigua and Barbuda Falcons",
    str_detect(team_name_vector, "St Kitts|St\\. Kitts") ~ "St Kitts and Nevis Patriots",
    str_detect(team_name_vector, "Guyana") ~ "Guyana Amazon Warriors",
    str_detect(team_name_vector, "Jamaica") ~ "Jamaica Tallawahs",
    str_detect(team_name_vector, "Barbados") ~ "Barbados Royals",
    str_detect(team_name_vector, "Trinbago") ~ "Trinbago Knight Riders",
    str_detect(team_name_vector, "St Lucia|St\\. Lucia|Saint Lucia") ~ "St Lucia Kings",
    TRUE ~ team_name_vector
  )
}

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

head_to_head_main <- function() {
  
  # Function to get head to head data-------------------------------------------#
  get_h2h <- function(market_table) {
    
    # Home Team Data
    home_info <- market_table[2, 1:2]
    names(home_info) <- c("home_team", "home_win")
    
    # Away Team Data
    away_info <- market_table[3, 1:2]
    names(away_info) <- c("away_team", "away_win")
    
    # Match Start Time
    match_start_time <- market_table[1, 1]
    names(match_start_time) <- "start_time"
    
    # Combine together into one table
    bind_cols(home_info, away_info, match_start_time)
    
  }
  
  # Map function to main markets list
  topsport_h2h <- map(main_markets, get_h2h) |> bind_rows()
  
  # Fix names
  topsport_h2h <-
    topsport_h2h |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TopSport") |> 
    mutate(start_time = dmy_hm(start_time)) |>
    filter(!is.na(home_win))
  
  # Write to csv
  write_csv(topsport_h2h, "Data/T20s/CPL/scraped_odds/topsport_h2h.csv")
}

head_to_head_main()

#===============================================================================
# Player Runs
#===============================================================================

# Function to read the html of a given url
read_topsport_html <- function(url) {
  
  # Get market name from url
  market_name <- str_extract(url, "(?<=Cricket/).*")
  market_name <- str_remove(market_name, "\\/.*$")
  
  # Get line from market name
  line <- str_extract(market_name, "\\d+")
  
  # Get match name from html
  match_name_html <-    
    url |> 
    read_html() |>
    html_nodes("h1") |>
    html_text() |> 
    paste(collapse = " ")
  
  # Get match name from extracted text
  match_name_html <- strsplit(match_name_html, split = " - ")
  match_name <- match_name_html[[1]][length(match_name_html[[1]])]
  player_name <- match_name_html[[1]][length(match_name_html[[1]]) - 1]
  
  # Get data from html
  result <-    
    url |> 
    read_html() |>
    html_nodes(".marketTable") |> 
    html_table()
  
  # Get tibble
  result[[1]] |>
    mutate(line = ifelse(!is.na(line), line, str_extract(Selection, "\\d+\\.\\d+"))) |>
    mutate(match = match_name) |>
    mutate(Selection = if_else(str_detect(Selection, "(Over)|(Under)"), paste(player_name, Selection), Selection))
}

# Get data for pick your own player runs----------------------------------------

# Get URLs
pick_your_own_runs_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Runs_Scored")]

# Map function
player_runs_alternate <-
  map(pick_your_own_runs_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(player_runs_alternate) == 0) {
  player_runs_alternate <-
    tibble(match = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

player_runs_alternate <- 
  player_runs_alternate |> 
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_runs_alternate <-
  player_runs_alternate |> 
  mutate(market_name = "Player Runs", agency = "TopSport") |>
  select(match, market_name, player_name, line, over_price, agency)

# Get data for player runs over/under-----------------------------------------

# Get URLs
player_runs_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Runs_.*\\(")]

# Map function
player_runs_lines <-
  map(player_runs_markets, read_topsport_html) |>
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(player_runs_lines) == 0) {
  player_runs_lines <-
    tibble(match = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

player_runs_lines <-
  player_runs_lines |> 
  mutate(line = as.numeric(line)) |>
  rename(over_price = Win) |>
  mutate(player_name = str_remove(Selection, " \\(.*\\).*")) |> 
  mutate(player_team = str_extract(Selection, "\\(.*\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)"))

# Get Overs
player_runs_lines_overs <-
  player_runs_lines |>
  filter(str_detect(Selection, "Over")) |>
  select(-Selection)

# Get Unders
player_runs_lines_unders <-
  player_runs_lines |>
  filter(str_detect(Selection, "Under")) |>
  rename(under_price = over_price) |>
  select(-Selection)

# Combine
player_runs_lines <-
  player_runs_lines_overs |>
  left_join(player_runs_lines_unders) |>
  mutate(market_name = "Player Runs") |>
  mutate(agency = "TopSport") |>
  select(match,
         market_name,
         player_name,
         player_team,
         line,
         over_price,
         under_price,
         agency)

# Write out all player runs
player_runs_all <- 
  player_runs_alternate |>
  bind_rows(player_runs_lines) |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(player_name, line) |> 
  distinct(match, player_name, player_team, opposition_team, line, over_price, under_price, .keep_all = TRUE)

player_runs_all |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_player_runs.csv")

#===============================================================================
# Player Wickets
#===============================================================================

# Get data for pick your own bowler wickets-------------------------------------

# Get URLs
pick_your_own_wickets_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Wicket[s]?_or_More")]

# Map function
player_wickets_alternate <-
  map(pick_your_own_wickets_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(player_wickets_alternate) == 0) {
  player_wickets_alternate <-
    tibble(match = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

player_wickets_alternate <-
  player_wickets_alternate |> 
  mutate(line = as.numeric(line) - 0.5) |>
  rename(over_price = Win) |>
  mutate(player_name = str_remove(Selection, " \\(.*\\).*")) |> 
  mutate(player_team = str_extract(Selection, "\\(.*\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)")) |> 
  mutate(market_name = "Player Wickets", agency = "TopSport") |>
  select(match, market_name, player_name, player_team, line, over_price, agency)

# Get data for player wickets over/under-----------------------------------------

# Get URLs
player_wickets_markets <-
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Wickets_.*\\(")]

# Map function
player_wickets_lines <-
  map(player_wickets_markets, read_topsport_html) |>
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(player_wickets_lines) == 0) {
  player_wickets_lines <-
    tibble(
      match = character(),
      start_date = character(),
      market_name = character(),
      Selection = character(),
      line = numeric(),
      Win = numeric(),
      agency = character()
    )
}

player_wickets_lines <-
  player_wickets_lines |>
  mutate(line = as.numeric(line)) |> 
  rename(over_price = Win) |>
  mutate(player_name = str_remove(Selection, " \\(.*\\).*")) |> 
  mutate(player_team = str_extract(Selection, "\\(.*\\)")) |>
  mutate(player_team = str_remove_all(player_team, "\\(|\\)")) |> 
  mutate(market_name = "Player Wickets", agency = "TopSport")

# Get Overs
player_wickets_lines_overs <-
  player_wickets_lines |>
  filter(str_detect(Selection, "Over")) |>
  select(match, market_name, player_name, player_team, line, over_price, agency)

# Get Unders
player_wickets_lines_unders <-
  player_wickets_lines |>
  filter(str_detect(Selection, "Under")) |>
  select(match, market_name, player_name, player_team, line, under_price = over_price, agency)

# Combine
player_wickets_lines <-
  player_wickets_lines_overs |>
  left_join(player_wickets_lines_unders)

# Write out all player wickets
player_wickets <- 
  player_wickets_alternate |>
  bind_rows(player_wickets_lines) |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  arrange(player_name, line) |> 
  distinct(match, player_name, player_team, opposition_team, line, over_price, under_price, .keep_all = TRUE)

player_wickets |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_player_wickets.csv")

#===============================================================================
# Boundaries
#===============================================================================

# Get data for pick your own fours----------------------------------------------

# Get URLs
pick_your_own_fours_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Player Fours")]

# Map function
player_fours_alternate <-
  map(pick_your_own_fours_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(player_fours_alternate) == 0) {
  player_fours_alternate <-
    tibble(match = character(),
           player_team = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

player_fours_alternate <-
  player_fours_alternate |>
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_fours_alternate <-
  player_fours_alternate |> 
  mutate(market_name = "Number of 4s", agency = "TopSport") |>
  select(match, start_date, market_name, player_name, player_team, line, over_price, agency)

# Get data for pick your own sixes----------------------------------------------

# Get URLs
pick_your_own_sixes_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Player Sixes")]

# Map function
player_sixes_alternate <-
  map(pick_your_own_sixes_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(player_sixes_alternate) == 0) {
  player_sixes_alternate <-
    tibble(match = character(),
           player_team = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

player_sixes_alternate <-
  player_sixes_alternate |>
  mutate(line = line - 0.5) |>
  rename(over_price = Win) |> 
  rename(player_name = Selection) |> 
  mutate(player_name = str_remove(player_name, " \\(.*\\)$")) |>
  relocate(match, .before = player_name)

# Add player team
player_sixes_alternate <-
  player_sixes_alternate |> 
  mutate(market_name = "Number of 6s", agency = "TopSport") |>
  select(match, start_date, market_name, player_name, player_team, line, over_price, agency)

# Write out all player boundaries
player_boundaries <- 
  player_fours_alternate |>
  bind_rows(player_sixes_alternate) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(agency = "TopSport") |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  select(
    match,
    market = market_name,
    home_team,
    away_team,
    player_name,
    player_team,
    opposition_team,
    line,
    over_price,
    agency
  ) |> 
  arrange(player_name, line) |> 
  distinct(match, player_name, player_team, opposition_team, line, over_price, .keep_all = TRUE)

player_boundaries |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_player_boundaries.csv")

#===============================================================================
# First Over Runs
#===============================================================================

# Get data for first over runs-------------------------------------------------

# Get URLs
first_over_runs_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Runs_In_First_Over")]

# Map function
first_over_runs <-
  map(first_over_runs_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(first_over_runs) == 0) {
  first_over_runs <-
    tibble(match = character(),
           player_team = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

first_over_runs <-
  first_over_runs |>
  mutate(line = as.numeric(line))

# Overs
first_over_runs_overs <-
  first_over_runs |>
  filter(!str_detect(Selection, "Under")) |>
  mutate(team = str_remove(Selection, ".* Total Runs In First Over ")) |> 
  mutate(team = str_remove(team, " Over .*")) |>
  mutate(market = "First Over Runs - Team", agency = "TopSport") |>
  select(match, team, market, line, over_price = Win, agency)

# Unders
first_over_runs_unders <-
  first_over_runs |>
  filter(str_detect(Selection, "Under")) |>
  mutate(team = str_remove(Selection, ".* Total Runs In First Over ")) |> 
  mutate(team = str_remove(team, " Under .*")) |>
  mutate(market = "First Over Runs - Team", agency = "TopSport") |>
  select(match, team, market, line, under_price = Win, agency)

# Combine
first_over_runs <- 
  left_join(first_over_runs_overs, first_over_runs_unders) |> 
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(team = fix_team_names(team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, team, market, line, over_price, under_price, agency)

# Write out first over runs
first_over_runs |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_first_over_runs.csv")

#===============================================================================
# Runs At Fall of First Wicket - Team
#===============================================================================

# Get data for first over runs-------------------------------------------------

# Get URLs
runs_at_first_wicket_team_runs_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Total_Runs_in_Opening_Partnership")]

# Map function
runs_at_first_wicket_team_runs <-
  map(runs_at_first_wicket_team_runs_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(runs_at_first_wicket_team_runs) == 0) {
  runs_at_first_wicket_team_runs <-
    tibble(match = character(),
           player_team = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

runs_at_first_wicket_team_runs <-
  runs_at_first_wicket_team_runs |>
  mutate(line = as.numeric(line)) |> 
  mutate(team = str_remove_all(Selection, "Upcoming Matches Total Runs in Opening Partnership ")) |> 
  mutate(team = str_remove_all(team, " Over.*$")) |>
  mutate(team = str_remove_all(team, " Under.*$"))

# Overs
runs_at_first_wicket_team_runs_overs <-
  runs_at_first_wicket_team_runs |>
  filter(!str_detect(Selection, "Under")) |>
  mutate(market = "Fall of 1st Wicket - Team", agency = "TopSport") |>
  select(match, market, team, line, over_price = Win, agency)

# Unders
runs_at_first_wicket_team_runs_unders <-
  runs_at_first_wicket_team_runs |>
  filter(str_detect(Selection, "Under")) |>
  mutate(market = "Fall of 1st Wicket - Team", agency = "TopSport") |>
  select(match, market, team, line, under_price = Win, agency)

# Combine
runs_at_first_wicket_team_runs_all <-
  runs_at_first_wicket_team_runs_overs |>
  left_join(runs_at_first_wicket_team_runs_unders) |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " v ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(team = fix_team_names(team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  select(match, market, team, line, over_price, under_price, agency)

# Write out
runs_at_first_wicket_team_runs_all |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_runs_at_first_wicket.csv")

#===============================================================================
# Most Team Wickets
#===============================================================================

# Get data for most team wickets -----------------------------------------------

# Get URLs
most_team_wickets_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "\\/Most_Wickets_")]

# Map function
most_team_wickets <-
  map(most_team_wickets_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(most_team_wickets) == 0) {
  most_team_wickets <-
    tibble(match = character(),
           player_team = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

# Tidy up
most_team_wickets <-
  most_team_wickets |>
  rename(price = Win) |>
  mutate(player_name = str_remove_all(Selection, " \\(.*$")) |> 
  separate(match, c("home_team", "away_team"), sep = " v ",remove = FALSE) |>
  mutate(team = str_extract(Selection, "\\(.*$")) |>
  mutate(player_team = str_remove_all(team, "[()]")) |> 
  mutate(opposition_team = ifelse(player_team == home_team, away_team, home_team)) |> 
  transmute(match,
            market = "Top Team Wicket Taker",
            home_team,
            away_team,
            player_name,
            player_team,
            opposition_team,
            price,
            agency = "TopSport") |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(player_team = fix_team_names(player_team)) |>
  mutate(opposition_team = as.character(opposition_team)) |> 
  mutate(opposition_team = fix_team_names(opposition_team)) |> 
  mutate(match = paste(home_team, "v", away_team))

# Write out
most_team_wickets |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_top_team_wicket_taker.csv")

#===============================================================================
# Highest Opening Partnership
#===============================================================================

# Get data for highest opening partnership-------------------------------------

# Get URLs
highest_opening_partnership_markets <- 
  topsport_other_markets[str_detect(topsport_other_markets, "Highest_Opening_Partnership")]

# Map function
highest_opening_partnership <-
  map(highest_opening_partnership_markets, read_topsport_html) |> 
  bind_rows()

# If nrow is zero make empty tibble
if (nrow(highest_opening_partnership) == 0) {
  highest_opening_partnership <-
    tibble(match = character(),
           player_team = character(),
           start_date = character(),
           market_name = character(),
           Selection = character(),
           line = numeric(),
           Win = numeric(),
           agency = character())
}

highest_opening_partnership <-
highest_opening_partnership |>  
  separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(Selection = fix_team_names(Selection)) |> 
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(market_name = "Highest Opening Partnership")

# Get home team price
highest_opening_partnership_home <-
  highest_opening_partnership |>
  filter(home_team == Selection) |> 
  transmute(match, home_team, away_team, market_name, home_price = Win)

# Get tie price
highest_opening_partnership_tie <-
  highest_opening_partnership |>
  filter(Selection == "Draw") |> 
  transmute(match, home_team, away_team, market_name, tie_price = Win)

# Get away team price
highest_opening_partnership_away <-
  highest_opening_partnership |>
  filter(away_team == Selection) |>
  transmute(match, home_team, away_team, market_name, away_price = Win)

# Combine
highest_opening_partnership <-
  highest_opening_partnership_home |>
  left_join(highest_opening_partnership_tie) |>
  left_join(highest_opening_partnership_away) |>
  mutate(agency = "TopSport")

# Write out
highest_opening_partnership |> 
  write_csv("Data/T20s/CPL/scraped_odds/topsport_highest_opening_partnership.csv")
