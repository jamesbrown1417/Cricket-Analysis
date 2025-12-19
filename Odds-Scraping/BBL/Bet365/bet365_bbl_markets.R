# Libraries
library(tidyverse)
library(rvest)

HTML_DIR <- "Odds-Scraping/BBL/Bet365/HTML"
OUTPUT_DIR <- "Data/T20s/Big Bash/scraped_odds"

list_scraped_files <- function(pattern) {
  list.files(HTML_DIR, full.names = TRUE, pattern = pattern)
}

map_scraped_files <- function(pattern, fn, safe = TRUE) {
  files <- list_scraped_files(pattern)
  if (!length(files)) {
    return(tibble())
  }
  
  if (safe) {
    results <-
      files |>
      map(safely(fn)) |>
      keep(~is.null(.x$error))
    
    if (!length(results)) {
      return(tibble())
    }
    
    results |> map(~.x$result) |> bind_rows()
  } else {
    files |> map(fn) |> bind_rows()
  }
}

extract_match_name <- function(doc) {
  doc |>
    html_node(".sph-EventWrapper_Label ") |>
    html_text() |>
    str_remove_all(" \\- .*")
}

fetch_market_block <- function(markets, market_names, label) {
  idx <- which(market_names == label)[1]
  markets[[idx]]
}

write_csv_if_rows <- function(df, path) {
  if (nrow(df) > 0) {
    write_csv(df, path)
  }
}

#===============================================================================
# Head to Head
#===============================================================================

get_head_to_head <- function() {
  scraped_file <- list_scraped_files("h2h")[1]
  if (is.na(scraped_file)) {
    return(tibble())
  }
  
  doc <- read_html(scraped_file)
  
  bet365_teams <-
    doc |>
    html_nodes(".rcl-ParticipantFixtureDetailsTeam_TeamName") |>
    html_text()
  
  bet365_h2h_odds <-
    doc |>
    html_nodes(".sgl-ParticipantOddsOnly80_Odds") |>
    html_text()
  
  home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
  home_odds <- bet365_h2h_odds[seq_along(home_teams)]
  home_odds_to_keep <- which(str_detect(home_odds, "^\\d+\\.\\d+$"))
  home_teams <- home_teams[home_odds_to_keep]
  home_odds <- home_odds[home_odds_to_keep]
  
  away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
  away_start <- length(home_teams) + 1
  away_end <- away_start + length(away_teams) - 1
  away_odds <- bet365_h2h_odds[away_start:away_end]
  away_odds_to_keep <- which(str_detect(away_odds, "^\\d+\\.\\d+$"))
  away_teams <- away_teams[away_odds_to_keep]
  away_odds <- away_odds[away_odds_to_keep]
  
  tibble(
    match = paste(home_teams, away_teams, sep = " v "),
    market = "Head To Head",
    home_team = home_teams,
    home_win = as.numeric(home_odds),
    away_team = away_teams,
    away_win = as.numeric(away_odds)
  ) |>
    mutate(margin = round((1/home_win + 1/away_win), 3)) |>
    mutate(agency = "Bet365")
}

#===============================================================================
# Player helpers
#===============================================================================

fix_batter_names <- function(player_name_vector) {
  case_when(
    str_detect(player_name_vector, "ADS Fletcher") ~ "Andre Fletcher",
    str_detect(player_name_vector, "BA King") ~ "Brandon King",
    str_detect(player_name_vector, "E Lewis") ~ "Evin Lewis",
    str_detect(player_name_vector, "F Zaman") ~ "Fakhar Zaman",
    str_detect(player_name_vector, "JP Greaves") ~ "Jamie Greaves",
    str_detect(player_name_vector, "KR Mayers") ~ "Kyle Mayers",
    str_detect(player_name_vector, "RR Rossouw") ~ "Rilee Rossouw",
    str_detect(player_name_vector, "T Bishop") ~ "Teddy Bishop",
    str_detect(player_name_vector, "K James") ~ "Kofi James",
    str_detect(player_name_vector, "SW Billings") ~ "Sam Billings",
    str_detect(player_name_vector, "SO Hetmyer") ~ "Shimron Hetmyer",
    str_detect(player_name_vector, "JJ Roy") ~ "Jason Roy",
    str_detect(player_name_vector, "M Deyal") ~ "Mark Deyal",
    str_detect(player_name_vector, "N Pooran") ~ "Nicholas Pooran",
    str_detect(player_name_vector, "T Stubbs") ~ "Tristan Stubbs",
    str_detect(player_name_vector, "TH David") ~ "Tim David",
    str_detect(player_name_vector, "M Nandu") ~ "Mikyle Nandu",
    str_detect(player_name_vector, "SD Hope") ~ "Shai Hope",
    str_detect(player_name_vector, "R Gurbaz") ~ "Rahmanullah Gurbaz",
    str_detect(player_name_vector, "Q de Kock") ~ "Quinton de Kock",
    str_detect(player_name_vector, "RRS Cornwall") ~ "Rahkeem Cornwall",
    str_detect(player_name_vector, "DA Miller") ~ "David Miller",
    str_detect(player_name_vector, "A Athanaze") ~ "Alick Athanaze",
    str_detect(player_name_vector, "F du Plessis") ~ "Faf du Plessis",
    str_detect(player_name_vector, "J Charles") ~ "Johnson Charles",
    str_detect(player_name_vector, "TL Seifert") ~ "Tim Seifert",
    str_detect(player_name_vector, "BR McDermott") ~ "Ben McDermott",
    str_detect(player_name_vector, "CP Jewell") ~ "Caleb Jewell",
    str_detect(player_name_vector, "J Fraser-McGurk") ~ "Jake Fraser-McGurk",
    str_detect(player_name_vector, "J Brown") ~ "Josh Brown",
    str_detect(player_name_vector, "L Evans") ~ "Laurie Evans",
    str_detect(player_name_vector, "JR Philippe") ~ "Josh Philippe",
    str_detect(player_name_vector, "M Bryant") ~ "Max Bryant",
    str_detect(player_name_vector, "M Labuschagne") ~ "Marnus Labuschagne",
    str_detect(player_name_vector, "M Renshaw") ~ "Matt Renshaw",
    str_detect(player_name_vector, "M Henriques") ~ "Moises Henriques",
    str_detect(player_name_vector, "M Stoinis") ~ "Marcus Stoinis",
    str_detect(player_name_vector, "M Louis") ~ "Mikyle Louis",
    str_detect(player_name_vector, "PBB Rajapaksa") ~ "Bhanuka Rajapaksa",
    str_detect(player_name_vector, "K Alleyne") ~ "Kadeem Alleyne",
    str_detect(player_name_vector, "MM Ali") ~ "Moeen Ali",
    str_detect(player_name_vector, "B Webster") ~ "Beau Webster",
    str_detect(player_name_vector, "KR Patterson") ~ "Kurtis Patterson",
    str_detect(player_name_vector, "JM Vince") ~ "James Vince",
    str_detect(player_name_vector, "MS Wade") ~ "Matthew Wade",
    str_detect(player_name_vector, "C Munro") ~ "Colin Munro",
    str_detect(player_name_vector, "CA Lynn") ~ "Chris Lynn",
    str_detect(player_name_vector, "DJM Short") ~ "D'Arcy Short",
    str_detect(player_name_vector, "JJ Peirson") ~ "Jimmy Peirson",
    str_detect(player_name_vector, "MW Short") ~ "Matt Short",
    str_detect(player_name_vector, "NA McSweeney") ~ "Nathan McSweeney",
    str_detect(player_name_vector, "C Connolly") ~ "Cooper Connolly",
    str_detect(player_name_vector, "KK Jennings") ~ "Keaton Jennings",
    str_detect(player_name_vector, "BM Duckett") ~ "Ben Duckett",
    str_detect(player_name_vector, "J Edwards") ~ "Jack Edwards",
    str_detect(player_name_vector, "MP Stoinis") ~ "Marcus Stoinis",
    str_detect(player_name_vector, "SB Harper") ~ "Sam Harper",
    TRUE ~ player_name_vector
  )
}

fix_bowler_names <- function(player_name_vector) {
  case_when(
    str_detect(player_name_vector, "A Hosein") ~ "Akeal Hosein",
    str_detect(player_name_vector, "A Joseph") ~ "Alzarri Joseph",
    str_detect(player_name_vector, "C Brathwaite") ~ "Carlos Brathwaite",
    str_detect(player_name_vector, "D Drakes") ~ "Dominic Drakes",
    str_detect(player_name_vector, "F Ahmed") ~ "Fabian Allen",
    str_detect(player_name_vector, "I Udana") ~ "Isuru Udana",
    str_detect(player_name_vector, "J Holder") ~ "Jason Holder",
    str_detect(player_name_vector, "K Cottoy") ~ "Kevin Sinclair",
    str_detect(player_name_vector, "K Paul") ~ "Keemo Paul",
    str_detect(player_name_vector, "K Pierre") ~ "Khary Pierre",
    str_detect(player_name_vector, "M Nabi") ~ "Mohammad Nabi",
    str_detect(player_name_vector, "O McCoy") ~ "Obed McCoy",
    str_detect(player_name_vector, "R Shepherd") ~ "Romario Shepherd",
    str_detect(player_name_vector, "S Cottrell") ~ "Sheldon Cottrell",
    str_detect(player_name_vector, "S Narine") ~ "Sunil Narine",
    str_detect(player_name_vector, "T Shamsi") ~ "Tabraiz Shamsi",
    str_detect(player_name_vector, "W Hasaranga") ~ "Wanindu Hasaranga",
    str_detect(player_name_vector, "C Green") ~ "Chris Green",
    str_detect(player_name_vector, "A Nortje") ~ "Anrich Nortje",
    str_detect(player_name_vector, "A Nedd") ~ "Ashmead Nedd",
    str_detect(player_name_vector, "AS Joseph") ~ "Alzarri Joseph",
    str_detect(player_name_vector, "D Pretorius") ~ "Dwaine Pretorius",
    str_detect(player_name_vector, "KR Mayers") ~ "Kyle Mayers",
    str_detect(player_name_vector, "G Motie") ~ "Gudakesh Motie",
    str_detect(player_name_vector, "S Joseph") ~ "Shamar Joseph",
    str_detect(player_name_vector, "SK Springer") ~ "Shamar Springer",
    str_detect(player_name_vector, "N Ahmad") ~ "Noor Ahmad",
    str_detect(player_name_vector, "I Wasim") ~ "Imad Wasim",
    str_detect(player_name_vector, "M Amir") ~ "Mohammad Amir",
    str_detect(player_name_vector, "M Clarke") ~ "McKenny Clarke",
    str_detect(player_name_vector, "MM Ali") ~ "Moeen Ali",
    str_detect(player_name_vector, "JO Holder") ~ "Jason Holder",
    str_detect(player_name_vector, "I Tahir") ~ "Imran Tahir",
    str_detect(player_name_vector, "K Maharaj") ~ "Keshav Maharaj",
    str_detect(player_name_vector, "OC McCoy") ~ "Obed McCoy",
    str_detect(player_name_vector, "MM Theekshana") ~ "Maheesh Theekshana",
    str_detect(player_name_vector, "A Zampa") ~ "Adam Zampa",
    str_detect(player_name_vector, "B Stanlake") ~ "Billy Stanlake",
    str_detect(player_name_vector, "F O'Neil") ~ "Fergus O'Neil",
    str_detect(player_name_vector, "KW Richardson") ~ "Kane Richardson",
    str_detect(player_name_vector, "N Ellis") ~ "Nathan Ellis",
    str_detect(player_name_vector, "RP Meredith") ~ "Riley Meredith",
    str_detect(player_name_vector, "TS Rogers") ~ "Tom Rogers",
    str_detect(player_name_vector, "W Salamkheil") ~ "Waqar Salamkheil",
    str_detect(player_name_vector, "B Dwarshuis") ~ "Ben Dwarshuis",
    str_detect(player_name_vector, "H Kerr") ~ "Hayden Kerr",
    str_detect(player_name_vector, "JM Bird") ~ "Jackson Bird",
    str_detect(player_name_vector, "SA Abbott") ~ "Sean Abbott",
    str_detect(player_name_vector, "CJ Boyce") ~ "Cameron Boyce",
    str_detect(player_name_vector, "HTRY Thornton") ~ "Henry Thornton",
    str_detect(player_name_vector, "J Overton") ~ "Jamie Overton",
    str_detect(player_name_vector, "L Pope") ~ "Lloyd Pope",
    str_detect(player_name_vector, "MJ Swepson") ~ "Mitchell Swepson",
    str_detect(player_name_vector, "MP Kuhnemann") ~ "Matthew Kuhnemann",
    str_detect(player_name_vector, "PI Walter") ~ "Paul Walter",
    str_detect(player_name_vector, "X Bartlett") ~ "Xavier Bartlett",
    str_detect(player_name_vector, "Adam Zampa") ~ "Adam Zampa",
    str_detect(player_name_vector, "AJ Tye") ~ "Andrew Tye",
    str_detect(player_name_vector, "Fergus O'Neil") ~ "Fergus O'Neill",
    str_detect(player_name_vector, "JA Richardson") ~ "Jhye Richardson",
    str_detect(player_name_vector, "JP Behrendorff") ~ "Jason Behrendorff",
    str_detect(player_name_vector, "Kane Richardson") ~ "Kane Richardson",
    str_detect(player_name_vector, "LR Morris") ~ "Lance Morris",
    str_detect(player_name_vector, "AF Milne") ~ "Adam Milne",
    str_detect(player_name_vector, "Ben Dwarshuis") ~ "Ben Dwarshuis",
    str_detect(player_name_vector, "PM Siddle") ~ "Peter Siddle",
    str_detect(player_name_vector, "Sean Abbott") ~ "Sean Abbott",
    str_detect(player_name_vector, "T Murphy") ~ "Todd Murphy",
    str_detect(player_name_vector, "TK Curran") ~ "Tom Curran",
    str_detect(player_name_vector, "U Mir") ~ "Usama Mir",
    TRUE ~ player_name_vector
  )
}

#===============================================================================
# Player Runs
#===============================================================================

parse_player_runs <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  batter_runs_block <- fetch_market_block(markets, market_names, "Batter Match Runs")
  batter_runs_cols <- batter_runs_block |> html_elements(".gl-Market_General")
  over_index <- which(str_detect(batter_runs_cols |> html_text(), "Over"))
  under_index <- which(str_detect(batter_runs_cols |> html_text(), "Under"))
  
  batter_match_runs <-
    tibble(
      player = batter_runs_block |> html_elements(".srb-ParticipantLabelWithTeam_Name") |> html_text(),
      team = batter_runs_block |> html_elements(".srb-ParticipantLabelWithTeam_Team") |> html_text(),
      line = batter_runs_cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Handicap") |> html_text() |> as.numeric(),
      over_price = batter_runs_cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      under_price = batter_runs_cols[[under_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric()
    )
  
  milestones_block <- fetch_market_block(markets, market_names, "Batter Milestones")
  milestones_cols <- milestones_block |> html_elements(".gl-Market_General")
  
  extract_milestone <- function(idx, line_value) {
    tibble(
      player = milestones_block |> html_elements(".srb-ParticipantLabelWithTeam_Name") |> html_text(),
      team = milestones_block |> html_elements(".srb-ParticipantLabelWithTeam_Team") |> html_text(),
      line = line_value,
      over_price = milestones_cols[[idx]] |> html_elements(".gl-ParticipantOddsOnly_Odds") |> html_text() |> as.numeric()
    )
  }
  
  milestone_map <- list(
    extract_milestone(which(str_detect(milestones_cols |> html_text(), "10\\+")), 9.5),
    extract_milestone(which(str_detect(milestones_cols |> html_text(), "20\\+")), 19.5),
    extract_milestone(which(str_detect(milestones_cols |> html_text(), "30\\+")), 29.5),
    extract_milestone(which(str_detect(milestones_cols |> html_text(), "50\\+")), 49.5),
    extract_milestone(which(str_detect(milestones_cols |> html_text(), "70\\+")), 69.5)
  )
  
  match_name <- extract_match_name(doc)
  
  bind_rows(
    batter_match_runs |>
      mutate(market = "Player Runs"),
    bind_rows(milestone_map) |>
      mutate(under_price = NA_real_, market = "Player Runs")
  ) |>
    arrange(player, line, over_price) |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
}

player_runs <-
  map_scraped_files("player", parse_player_runs) |>
  mutate(player = fix_batter_names(player)) |>
  rename(player_name = player, player_team = team)

write_csv_if_rows(player_runs, file.path(OUTPUT_DIR, "bet365_player_runs.csv"))

#===============================================================================
# Player Boundaries
#===============================================================================

parse_player_boundaries <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  extract_boundary_table <- function(label, market_value) {
    block <- fetch_market_block(markets, market_names, label)
    cols <- block |> html_elements(".gl-Market_General")
    over_index <- which(str_detect(cols |> html_text(), "Over"))
    under_index <- which(str_detect(cols |> html_text(), "Under"))
    
    tibble(
      player = block |> html_elements(".srb-ParticipantLabelWithTeam_Name") |> html_text(),
      team = block |> html_elements(".srb-ParticipantLabelWithTeam_Team") |> html_text(),
      line = cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Handicap") |> html_text() |> as.numeric(),
      over_price = cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      under_price = cols[[under_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      market = market_value
    )
  }
  
  match_name <- extract_match_name(doc)
  
  bind_rows(
    extract_boundary_table("Batter Total Match Fours", "Batter Match fours"),
    extract_boundary_table("Batter Total Match Sixes", "Batter Match sixes")
  ) |>
    arrange(player, line, over_price) |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
}

player_boundaries <-
  map_scraped_files("player", parse_player_boundaries) |>
  mutate(player = fix_batter_names(player)) |>
  rename(player_name = player, player_team = team)

write_csv_if_rows(player_boundaries, file.path(OUTPUT_DIR, "bet365_player_boundaries.csv"))

#===============================================================================
# Player Wickets
#===============================================================================

parse_player_wickets <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  wickets_block <- fetch_market_block(markets, market_names, "Bowler Total Match Wickets")
  wickets_cols <- wickets_block |> html_elements(".gl-Market_General")
  over_index <- which(str_detect(wickets_cols |> html_text(), "Over"))
  under_index <- which(str_detect(wickets_cols |> html_text(), "Under"))
  
  bowler_match_wickets <-
    tibble(
      player = wickets_block |> html_elements(".srb-ParticipantLabelWithTeam_Name") |> html_text(),
      team = wickets_block |> html_elements(".srb-ParticipantLabelWithTeam_Team") |> html_text(),
      line = wickets_cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Handicap") |> html_text() |> as.numeric(),
      over_price = wickets_cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      under_price = wickets_cols[[under_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      market = "Player Wickets"
    )
  
  milestones_block <- fetch_market_block(markets, market_names, "Bowler Milestones")
  milestones_cols <- milestones_block |> html_elements(".gl-Market_General")
  
  make_milestone <- function(pattern, line_value) {
    idx <- which(str_detect(milestones_cols |> html_text(), pattern))
    tibble(
      player = milestones_block |> html_elements(".srb-ParticipantLabelWithTeam_Name") |> html_text(),
      team = milestones_block |> html_elements(".srb-ParticipantLabelWithTeam_Team") |> html_text(),
      line = line_value,
      over_price = milestones_cols[[idx]] |> html_elements(".gl-ParticipantOddsOnly_Odds") |> html_text() |> as.numeric(),
      under_price = NA_real_,
      market = "Player Wickets"
    )
  }
  
  match_name <- extract_match_name(doc)
  
  bind_rows(
    bowler_match_wickets,
    make_milestone("1\\+", 0.5),
    make_milestone("2\\+", 1.5),
    make_milestone("3\\+", 2.5),
    make_milestone("4\\+", 3.5)
  ) |>
    arrange(player, line, over_price) |>
    mutate(match = match_name) |>
    relocate(match, .before = player)
}

player_wickets <-
  map_scraped_files("player", parse_player_wickets) |>
  mutate(player = fix_bowler_names(player)) |>
  rename(player_name = player, player_team = team)

write_csv_if_rows(player_wickets, file.path(OUTPUT_DIR, "bet365_player_wickets.csv"))

#===============================================================================
# Match Totals (Fours/Sixes/Boundaries)
#===============================================================================

parse_match_totals <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  totals_block <- fetch_market_block(markets, market_names, "Match Totals")
  totals_cols <- totals_block |> html_elements(".gl-Market_General")
  over_index <- which(str_detect(totals_cols |> html_text(), "Over"))
  under_index <- which(str_detect(totals_cols |> html_text(), "Under"))
  
  match_name <- extract_match_name(doc)
  
  tibble(
    market = totals_block |> html_elements(".srb-ParticipantLabel_Name") |> html_text(),
    line = totals_cols[[over_index]] |> html_elements(".srb-ParticipantCenteredStackedMarketRow_Handicap") |> html_text() |> as.numeric(),
    over_price = totals_cols[[over_index]] |> html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |> html_text() |> as.numeric(),
    under_price = totals_cols[[under_index]] |> html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |> html_text() |> as.numeric(),
    match = match_name,
    agency = "Bet365"
  ) |>
    relocate(match, .before = market)
}

match_totals <- map_scraped_files("body_html_match", parse_match_totals)

total_match_fours <-
  match_totals |>
  filter(market == "Fours") |>
  mutate(market = "Total Match Fours")

total_match_sixes <-
  match_totals |>
  filter(market == "Sixes") |>
  mutate(market = "Total Match Sixes")

total_match_boundaries <-
  match_totals |>
  filter(market == "Boundaries") |>
  mutate(market = "Total Match Boundaries")

write_csv_if_rows(total_match_fours, file.path(OUTPUT_DIR, "bet365_total_match_fours.csv"))
write_csv_if_rows(total_match_sixes, file.path(OUTPUT_DIR, "bet365_total_match_sixes.csv"))
write_csv_if_rows(total_match_boundaries, file.path(OUTPUT_DIR, "bet365_total_match_boundaries.csv"))

#===============================================================================
# Team Totals (Fours/Sixes)
#===============================================================================

parse_team_totals <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  make_table <- function(label, market_value) {
    block <- fetch_market_block(markets, market_names, label)
    cols <- block |> html_elements(".gl-Market_General")
    over_index <- which(str_detect(cols |> html_text(), "Over"))
    under_index <- which(str_detect(cols |> html_text(), "Under"))
    
    tibble(
      team = block |> html_elements(".srb-ParticipantLabel_Name") |> html_text(),
      line = cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Handicap") |> html_text() |> as.numeric(),
      over_price = cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      under_price = cols[[under_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
      market = market_value,
      agency = "Bet365"
    )
  }
  
  match_name <- extract_match_name(doc)
  
  bind_rows(
    make_table("Team Total Match Fours", "Total Team Fours"),
    make_table("Team Total Match Sixes", "Total Team Sixes")
  ) |>
    mutate(match = match_name) |>
    relocate(match, .before = market)
}

team_totals <- map_scraped_files("body_html_team", parse_team_totals)

total_team_fours <- team_totals |> filter(market == "Total Team Fours")
total_team_sixes <- team_totals |> filter(market == "Total Team Sixes")

write_csv_if_rows(total_team_fours, file.path(OUTPUT_DIR, "bet365_team_fours.csv"))
write_csv_if_rows(total_team_sixes, file.path(OUTPUT_DIR, "bet365_team_sixes.csv"))

#===============================================================================
# First Over Runs
#===============================================================================

parse_first_over_runs <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  block <- fetch_market_block(markets, market_names, "1st Over of Match - Total Runs - Team")
  cols <- block |> html_elements(".gl-Market_General")
  over_index <- which(str_detect(cols |> html_text(), "Over"))
  under_index <- which(str_detect(cols |> html_text(), "Under"))
  
  match_name <- extract_match_name(doc)
  
  tibble(
    team = block |> html_elements(".srb-ParticipantLabel") |> html_text(),
    line = cols[[over_index]] |> html_elements(".srb-ParticipantCenteredStackedMarketRow_Handicap") |> html_text() |> as.numeric(),
    over_price = cols[[over_index]] |> html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |> html_text() |> as.numeric(),
    under_price = cols[[under_index]] |> html_elements(".srb-ParticipantCenteredStackedMarketRow_Odds") |> html_text() |> as.numeric(),
    market = "First Over Runs - Team",
    match = match_name,
    agency = "Bet365"
  ) |>
    relocate(match, .before = market)
}

first_over_runs <-
  map_scraped_files("first_over", parse_first_over_runs, safe = FALSE)

write_csv_if_rows(first_over_runs, file.path(OUTPUT_DIR, "bet365_first_over_runs.csv"))

#===============================================================================
# Fall of First Wicket
#===============================================================================

parse_fall_of_first_wicket <- function(scraped_file) {
  doc <- read_html(scraped_file)
  markets <- doc |> html_nodes(".gl-MarketGroupPod")
  market_names <- markets |> html_elements(".cm-MarketGroupWithIconsButton_Text ") |> html_text()
  
  block <- fetch_market_block(markets, market_names, "Team - Opening Partnership Total")
  cols <- block |> html_elements(".gl-Market_General")
  over_index <- which(str_detect(cols |> html_text(), "Over"))
  under_index <- which(str_detect(cols |> html_text(), "Under"))
  
  match_name <- extract_match_name(doc)
  
  tibble(
    team = block |> html_elements(".srb-ParticipantLabel_Name ") |> html_text(),
    line = cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Handicap") |> html_text() |> as.numeric(),
    over_price = cols[[over_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
    under_price = cols[[under_index]] |> html_elements(".gl-ParticipantCenteredStacked_Odds") |> html_text() |> as.numeric(),
    market = "Fall of 1st Wicket - Team",
    match = match_name,
    agency = "Bet365"
  ) |>
    relocate(match, .before = market)
}

fall_of_first_wicket <-
  map_scraped_files("body_html_team", parse_fall_of_first_wicket)

write_csv_if_rows(fall_of_first_wicket, file.path(OUTPUT_DIR, "bet365_runs_at_first_wicket.csv"))

#===============================================================================
# Head to head export
#===============================================================================

bet365_h2h <- get_head_to_head()
write_csv_if_rows(bet365_h2h, file.path(OUTPUT_DIR, "bet365_h2h.csv"))
