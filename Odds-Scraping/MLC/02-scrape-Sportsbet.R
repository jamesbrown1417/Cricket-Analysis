# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/cricket/major-league-cricket"

# Get player metadata
player_meta_updated <- read_rds("Data/player_meta_updated.rds")

# Get player names and countries
player_names <-
  player_meta_updated |> 
  filter(dob >= "1980-01-01") |> 
  select(unique_name, full_name, country) |> 
  mutate(country = if_else(country == "U.S.A.", "USA", country)) |>
  # Split full name into first, middle and last names
  separate(unique_name, c("first_name", "last_name"), sep = " ", remove = FALSE, extra = "merge") |> 
  mutate(initials_join_name = paste(substr(first_name, 1, 1), last_name, sep = " ")) |> 
  mutate(initials_join_name = case_when(unique_name == "PHKD Mendis" ~ "K Mendis",
                                        TRUE ~ initials_join_name))

# Separate out middle names
separated_names <-
  player_names %>%
  mutate(
    first_name = sapply(strsplit(full_name, " "), `[`, 1),  # First element is the first name
    last_name = sapply(strsplit(full_name, " "), tail, 1), # Last element is the last name
    middle_names = sapply(strsplit(full_name, " "), function(x) {
      if (length(x) > 2) {
        paste(x[-c(1, length(x))], collapse=" ") # Collapse all but the first and last elements
      } else {
        NA  # No middle name
      }
    })
  ) |> 
  mutate(full_join_name = paste(first_name, last_name, sep = " ")) |> 
  distinct(full_join_name, .keep_all = TRUE) |>
  mutate(full_join_name = case_when(full_join_name == "Quinton Kock" ~ "Quinton de Kock",
                                    full_join_name == "Pasqual Mendis" ~ "Kamindu Mendis",
                                    full_join_name == "Pathum Silva" ~ "Pathum Nissanka",
                                    full_join_name == "Dhananjaya Silva" ~ "Dhananjaya De Silva",
                                    full_join_name == "Michael Lingen" ~ "Michael Van Lingen",
                                    full_join_name == "Shakib Hasan" ~ "Shakib Al Hasan",
                                    full_join_name == "Tanzid Tamim" ~ "Tanzid Hasan",
                                    full_join_name == "Najmul Shanto" ~ "Najmul Hossain Shanto",
                                    full_join_name == "Noor Lakanwal" ~ "Noor Ahmad",
                                    full_join_name == "Naveen-ul-Haq Murid" ~ "Naveen-ul-Haq",
                                    TRUE ~ full_join_name))

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

main_markets_function <- function() {
  
  # Get data from main market page
  matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
  }
  
  # Function to get odds
  get_odds <- function(match) {
    odds <-
      match |>
      html_nodes(".priceTextSize_frw9zm9") |>
      html_text() |>
      as.numeric()
    
    # Home team
    home_win <- odds[1]
    away_win <- odds[2]
    
    # Output
    tibble(home_win, away_win)
  }
  
  # Function to get start time
  get_start_time <- function(match) {
    start_time <-
      match |>
      html_nodes(".oneLine_f15ay66x") |>
      html_text()
    
    # Output
    tibble(start_time)
  }
  
  # Map functions to each match and combine together
  all_main_market_data <-
    bind_cols(
      map(matches, get_team_names) |> bind_rows(),
      map(matches, get_odds) |> bind_rows(),
      map(matches, get_start_time) |> bind_rows()
    )
  
  #===============================================================================
  # Head to Head markets---------------------------------------------------------#
  #===============================================================================
  
  sportsbet_h2h <-
    all_main_market_data |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet")
  
  # Write to csv
  write_csv(sportsbet_h2h, "Data/T20s/Major League Cricket/scraped_odds/sportsbet_h2h.csv")
  
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

player_props_function <- function() {
  # Function to get team names
  get_team_names <- function(match) {
    team_names <-
      match |>
      html_nodes(".participantText_fivg86r") |>
      html_text()
    
    # Home team and Away Team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
  }
  
  
  # Get match links
  match_links <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".linkMultiMarket_fcmecz0") |>
    html_attr("href")
  
  # Get match IDs from links
  match_ids <-
    match_links |>
    str_extract("\\d{4,10}$") |>
    as.numeric()
  
  # Get data from main market page
  matches <-
    sportsbet_url |>
    read_html() |>
    html_nodes(".White_fqa53j6")
  
  # Get team names that correspond to each match link
  team_names <-
    map_dfr(matches, get_team_names) |>
    bind_cols("match_id" = match_ids)
  
  # Get all links
  top_markets_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/304/Markets"
    )
  
  run_scorer_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/306/Markets"
    )
  
  top_wicket_takers_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/307/Markets"
    )
  
  match_markets_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/326/Markets"
    )
  
  team_totals_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/328/Markets"
    )
  
  first_innings_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/325/Markets"
    )
  
  x_over_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/658/Markets"
    )
  
  fall_of_wicket_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/435/Markets"
    )
  
  dismissal_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/308/Markets"
    )
  
  player_boundaries_links <-
    glue(
      "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/429/Markets"
    )
  
  # Function to read a url and get the player props-------------------------------
  
  read_prop_url <- function(url) {
    # Make request and get response
    sb_response <-
      request(url) |>
      req_perform() |>
      resp_body_json()
    
    # Empty vectors to append to
    prop_market_name = c()
    selection_name_prop = c()
    prop_market_selection = c()
    prop_market_price = c()
    player_id = c()
    market_id = c()
    handicap = c()
    
    # Loop through each market
    for (market in sb_response) {
      for (selection in market$selections) {
        # Append to vectors
        prop_market_name = c(prop_market_name, market$name)
        selection_name_prop = c(selection_name_prop, selection$name)
        prop_market_selection = c(prop_market_selection, selection$resultType)
        player_id = c(player_id, selection$externalId)
        market_id = c(market_id, market$externalId)
        
        if (is.null(selection$unformattedHandicap)) {
          selection$unformattedHandicap = NA
          handicap = c(handicap, selection$unformattedHandicap)
        } else {
          selection$unformattedHandicap = as.numeric(selection$unformattedHandicap)
          handicap = c(handicap, selection$unformattedHandicap)
        }
        
        if (is.null(selection$price$winPrice)) {
          selection$price$winPrice = NA
          prop_market_price = c(prop_market_price, selection$price$winPrice)
        } else {
          selection$price$winPrice = as.numeric(selection$price$winPrice)
          prop_market_price = c(prop_market_price, selection$price$winPrice)
        }
      }
    }
    
    # Output
    tibble(
      prop_market_name,
      selection_name_prop,
      prop_market_selection,
      prop_market_price,
      player_id,
      market_id,
      handicap,
      url
    )
  }
  
  # Safe version that just returns NULL if there is an error
  safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)
  
  #===============================================================================
  # Player Runs
  #===============================================================================
  
  # Map function to player points urls
  player_runs_data <-
    map(run_scorer_links, safe_read_prop_url)
  
  # Get just result part from output
  player_runs_data <-
    player_runs_data |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Get match top run scorer
  match_top_run_scorer <-
    player_runs_data |>
    filter(str_detect(prop_market_name, "Top Match Batter")) |>
    transmute(
      match,
      market = "Top Match Run Scorer",
      home_team,
      away_team,
      player_name = selection_name_prop,
      price = prop_market_price
    )
  
  # Player runs over / under
  player_runs_overs <-
    player_runs_data |>
    filter(str_detect(prop_market_name, "Total Runs")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Player Runs",
      home_team,
      away_team,
      player_name = str_remove(prop_market_name, " Total Runs \\- .*"),
      line = handicap,
      over_price = prop_market_price
    )
  
  player_runs_unders <-
    player_runs_data |>
    filter(str_detect(prop_market_name, "Total Runs")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Player Runs",
      home_team,
      away_team,
      player_name = str_remove(prop_market_name, " Total Runs \\- .*"),
      line = handicap,
      under_price = prop_market_price
    )

  player_runs_combined <-
    player_runs_overs |>
    full_join(
      player_runs_unders,
      by = c(
        "match",
        "market",
        "home_team",
        "away_team",
        "player_name",
        "line"
      )
    ) |>
    mutate(
      player_name = case_when(
        player_name == "Gerhard Erasmus" ~ "Merwe Erasmus",
        player_name == "D De Silva" ~ "D de Silva",
        player_name == "S Ssesazi" ~ "S Sesazi",
        player_name == "Will Jacks" ~ "William Jacks",
        player_name == "Phil Salt" ~ "Philip Salt",
        player_name == "Jos Buttler" ~ "Joseph Buttler",
        player_name == "George Munsey" ~ "Henry Munsey",
        player_name == "Max ODowd" ~ "Maxwell O'Dowd",
        player_name == "S Smrwickrma" ~ "Wedagedara Samarawickrama",
        player_name == "Jonny Bairstow" ~ "Jonathan Bairstow",
        player_name == "Ollie Hairs" ~ "Oliver Hairs",
        player_name == "Richie Berrington" ~ "Richard Berrington",
        player_name == "JJ Smit" ~ "Johannes Smit",
        player_name == "Jan Nicolaas Frylinck" ~ "Jan Frylinck",
        player_name == "Matty Cross" ~ "Matthew Cross",
        player_name == "Niko Davin" ~ "Nikolaas Davin",
        .default = player_name
      )
    ) |>
    mutate(agency = "Sportsbet")
  
  # Player runs alternative lines
  player_runs_alternative_lines <-
    player_runs_data |>
    filter(str_detect(prop_market_name, "To Score")) |>
    mutate(prop_market_name = str_replace(prop_market_name, "Fifty", "50")) |>
    transmute(
      match,
      market = "Player Runs",
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " - Over"),
      line = str_extract(prop_market_name, "[0-9]{1,3}"),
      over_price = prop_market_price
    ) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(agency = "Sportsbet") |>
    mutate(
      player_name = case_when(
        player_name == "G Erasmus" ~ "M Erasmus",
        player_name == "D De Silva" ~ "D de Silva",
        player_name == "S Ssesazi" ~ "S Sesazi",
        player_name == "Will Jacks" ~ "William Jacks",
        player_name == "Phil Salt" ~ "Philip Salt",
        player_name == "Jos Buttler" ~ "Joseph Buttler",
        player_name == "George Munsey" ~ "Henry Munsey",
        player_name == "Max ODowd" ~ "Maxwell O'Dowd",
        player_name == "S Smrwickrma" ~ "Wedagedara Samarawickrama",
        player_name == "Jonny Bairstow" ~ "Jonathan Bairstow",
        player_name == "Ollie Hairs" ~ "Oliver Hairs",
        player_name == "Richie Berrington" ~ "Richard Berrington",
        player_name == "JJ Smit" ~ "Johannes Smit",
        player_name == "Jan Nicolaas Frylinck" ~ "Jan Frylinck",
        player_name == "Matty Cross" ~ "Matthew Cross",
        player_name == "Niko Davin" ~ "Nikolaas Davin",
        .default = player_name
      )
    )
  
  # Combine all
  player_runs_all <-
    bind_rows(player_runs_combined, player_runs_alternative_lines) |>
    arrange(player_name, line) # |> 
    # left_join(separated_names[, c("full_join_name", "unique_name", "country")], by = c("player_name" = "full_join_name")) |>
    # select(-player_name) |>
    # relocate(player_team, opposition_team, .after = player_name)
  
  # Write to csv----------------------------------------------------------------
  write_csv(match_top_run_scorer,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_match_top_run_scorer.csv")
  
  write_csv(player_runs_all,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_player_runs.csv")
  
  #===============================================================================
  # Top Wicket Takers
  #===============================================================================
  
  # Map function to player points urls
  top_wicket_takers_data <-
    map(top_wicket_takers_links, safe_read_prop_url)

  # Get just result part from output
  top_wicket_takers_data <-
    top_wicket_takers_data |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))

  # Get home team top wicket taker
  home_top_wicket_taker <-
    top_wicket_takers_data |>
    filter(str_detect(prop_market_name, "Top .* Wicket Taker")) |>
    mutate(player_team = str_remove(prop_market_name, "Top ")) |>
    mutate(player_team = str_remove(player_team, " Wicket Taker \\(20 Overs\\)")) |>
    filter(player_team == home_team) |>
    transmute(
      match,
      market = "Top Team Wicket Taker",
      home_team,
      away_team,
      player_name = selection_name_prop,
      player_team,
      opposition_team = away_team,
      price = prop_market_price
    )

  # Get away team top  wicket taker
  away_top_wicket_taker <-
    top_wicket_takers_data |>
    filter(str_detect(prop_market_name, "Top .* Wicket Taker")) |>
    mutate(player_team = str_remove(prop_market_name, "Top ")) |>
    mutate(player_team = str_remove(player_team, " Wicket Taker \\(20 Overs\\)")) |>
    filter(player_team == away_team) |>
    transmute(
      match,
      market = "Top Team Wicket Taker",
      home_team,
      away_team,
      player_name = selection_name_prop,
      player_team,
      opposition_team = home_team,
      price = prop_market_price
    )

  # Combine
  top_team_wicket_taker <-
    bind_rows(home_top_wicket_taker, away_top_wicket_taker) |>
    mutate(
      player_name = case_when(player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
                              .default = player_name)
    )

  # Alternate Player Wickets
  player_wickets_alternative_lines <-
    top_wicket_takers_data |>
    filter(str_detect(prop_market_name, "to Take")) |>
    transmute(
      match,
      market = "Player Wickets",
      home_team,
      away_team,
      player_name = str_remove(selection_name_prop, " - Over"),
      line = str_extract(prop_market_name, "[0-9]{1,3}"),
      over_price = prop_market_price
    ) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(agency = "Sportsbet") |>
    mutate(
      player_name = case_when(
        player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
        player_name == "Matthew Kuhnemann" ~ "Matt Kuhnemann",
        player_name == "Mujeeb Ur-Rahman" ~ "Mujeeb Ur Rahman",
        player_name == "Stephen O'Keefe" ~ "Steve O'Keefe",
        player_name == "Tom Rogers." ~ "Tom Rogers",
        .default = player_name
      )
    ) # |>
    # left_join(player_teams[, c("player_name", "player_team")], by = "player_name") |>
    # mutate(
    #   opposition_team = case_when(
    #     player_team == home_team ~ away_team,
    #     player_team == away_team ~ home_team
    #   )
    # ) |>
    # relocate(player_team, opposition_team, .after = player_name)

  # Write to csv----------------------------------------------------------------
  write_csv(
    player_wickets_alternative_lines,
    "Data/T20s/Major League Cricket/scraped_odds/sportsbet_player_wickets.csv"
  )
  
  write_csv(
    top_team_wicket_taker,
    "Data/T20s/Major League Cricket/scraped_odds/sportsbet_top_team_wicket_taker.csv"
  )
  
  #===============================================================================
  # Team Totals
  #===============================================================================
  
  # Map function to player points urls
  team_totals <-
    map(team_totals_links, safe_read_prop_url)
  
  # Get just result part from output
  team_totals <-
    team_totals |>
    map("result") |>
    map_df(bind_rows)
  
    if(nrow(team_totals) == 0) {
      team_totals <- tibble(url = "", prop_market_name = "", selection_name_prop = "", prop_market_price = "")
    }
  
    team_totals <-
    team_totals |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Team Totals-----------------------------------------------------------------
  team_totals_overs <-
    team_totals |>
    filter(str_detect(prop_market_name, "Total Runs")) |> 
    filter(str_detect(selection_name_prop, "Over"))
  
  #===============================================================================
  # Match Markets
  #===============================================================================
  
  # Map function to player points urls
  match_markets <-
    map(match_markets_links, safe_read_prop_url)
  
  # Get just result part from output
  match_markets <-
    match_markets |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Match Fours-----------------------------------------------------------------
  match_fours_overs <-
    match_markets |>
    filter(str_detect(prop_market_name, "^Total Match Fours")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Total Match Fours",
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  match_fours_unders <-
    match_markets |>
    filter(str_detect(prop_market_name, "^Total Match Fours")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Total Match Fours",
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  match_fours <-
    match_fours_overs |>
    left_join(match_fours_unders, by = c("match", "home_team", "away_team", "line", "market")) |>
    mutate(agency = "Sportsbet")
  
  # Match Sixes-----------------------------------------------------------------
  match_sixes_overs <-
    match_markets |>
    filter(str_detect(prop_market_name, "^Total Match Sixes")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Total Match Sixes",
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  match_sixes_unders <-
    match_markets |>
    filter(str_detect(prop_market_name, "^Total Match Sixes")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Total Match Sixes",
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  match_sixes <-
    match_sixes_overs |>
    left_join(match_sixes_unders, by = c("match", "home_team", "away_team", "line", "market")) |>
    mutate(agency = "Sportsbet")
  
  # Team Sixes------------------------------------------------------------------
  team_sixes_overs <-
    match_markets |>
    filter(str_detect(prop_market_name, "[AZ]* Total Sixes")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Total Team Sixes",
      home_team,
      away_team,
      team = str_remove(prop_market_name, " Total Sixes .*"),
      line = handicap,
      over_price = prop_market_price
    )
  
  team_sixes_unders <-
    match_markets |>
    filter(str_detect(prop_market_name, "[AZ]* Total Sixes")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Total Team Sixes",
      home_team,
      away_team,
      team = str_remove(prop_market_name, " Total Sixes .*"),
      line = handicap,
      under_price = prop_market_price
    )
   
  team_sixes <-
    team_sixes_overs |>
    left_join(team_sixes_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
    mutate(agency = "Sportsbet")
  
  # Team Fours------------------------------------------------------------------
  team_fours_overs <-
    match_markets |>
    filter(str_detect(prop_market_name, "[AZ]* Total Fours")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Total Team Fours",
      home_team,
      away_team,
      team = str_remove(prop_market_name, " Total Fours .*"),
      line = handicap,
      over_price = prop_market_price
    )
  
  team_fours_unders <-
    match_markets |>
    filter(str_detect(prop_market_name, "[AZ]* Total Fours")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Total Team Fours",
      home_team,
      away_team,
      team = str_remove(prop_market_name, " Total Fours .*"),
      line = handicap,
      under_price = prop_market_price
    )
  
  team_fours <-
    team_fours_overs |>
    left_join(team_fours_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
    mutate(agency = "Sportsbet")
  
  # Team Totals-----------------------------------------------------------------
  team_totals_overs <-
    match_markets |>
    filter(str_detect(prop_market_name, "To Score Runs")) |>
    transmute(
      match,
      market = "Total Team Runs",
      home_team,
      away_team,
      team = str_remove(prop_market_name, " To Score Runs"),
      line = as.numeric(str_extract(selection_name_prop, "[0-9]{1,3}")) - 0.5,
      over_price = prop_market_price,
      agency = "Sportsbet"
    )
  
  # Write out data-------------------------------------------------------------
  write_csv(team_totals_overs,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_team_totals_overs.csv")
  
  write_csv(team_fours,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_team_fours.csv")
  
  write_csv(team_sixes,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_team_sixes.csv")
  
  write_csv(match_sixes,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_match_sixes.csv")
  
  write_csv(match_fours,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_match_fours.csv")
  
  #=============================================================================
  # X Overs
  #=============================================================================
  
  # Map function to player points urls
  x_over_markets <-
    map(x_over_links, safe_read_prop_url)
  
  # Get just result part from output
  x_over_markets <-
    x_over_markets |>
    map("result") |>
    map_df(bind_rows)
  
  # If nrow is zero make empty tibble
  if (nrow(x_over_markets) == 0) {
    x_over_markets <-
      tibble(match = character(),
             start_date = character(),
             market_name = character(),
             selection_name_prop = character(),
             prop_market_name = character(),
             handicap = numeric(),
             prop_market_price = numeric(),
             url = character(),
             agency = character())
  }
  
  x_over_markets <-
    x_over_markets |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Get First Over Runs
  first_over_runs_overs <-
    x_over_markets |>
    filter(str_detect(prop_market_name, "1st Over Total")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "First Over Runs",
      team = str_remove(prop_market_name, " 1st Over Total .*"),
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  first_over_runs_unders <-
    x_over_markets |>
    filter(str_detect(prop_market_name, "1st Over Total")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "First Over Runs",
      team = str_remove(prop_market_name, " 1st Over Total .*"),
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  first_over_runs_all <-
    first_over_runs_overs |>
    left_join(first_over_runs_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
    mutate(agency = "Sportsbet") |> 
    select(-home_team, -away_team)
  
  # Write out data-------------------------------------------------------------
  write_csv(first_over_runs_all,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_first_over_runs.csv")
  
  #=============================================================================
  # Fall of Wicket
  #=============================================================================
  
  # Map function to fall of wicket URLs
  fall_of_wicket_markets <-
    map(fall_of_wicket_links, safe_read_prop_url)
  
  # Get just result part from output
  fall_of_wicket_markets <-
    fall_of_wicket_markets |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # Get Overs
  fall_of_wicket_overs <-
    fall_of_wicket_markets |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Fall of 1st Wicket",
      team = str_remove(prop_market_name, " Runs at Fall of .*"),
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  fall_of_wicket_unders <-
    fall_of_wicket_markets |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Fall of 1st Wicket",
      team = str_remove(prop_market_name, " Runs at Fall of .*"),
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  fall_of_wicket_all <-
    fall_of_wicket_overs |>
    left_join(fall_of_wicket_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
    mutate(agency = "Sportsbet")
  
  # Write out data-------------------------------------------------------------
  write_csv(fall_of_wicket_all,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_runs_at_first_wicket.csv")
  
  #===============================================================================
  # First Innings
  #===============================================================================
  
  # Map function to player points urls
  first_innings_data <-
    map(first_innings_links, safe_read_prop_url)
  
  # Get just result part from output
  first_innings_data <-
    first_innings_data |>
    map("result") |>
    map_df(bind_rows) |>
    mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
    rename(match_id = url) |>
    mutate(match_id = as.numeric(match_id)) |>
    left_join(team_names, by = "match_id") |>
    mutate(match = paste(home_team, "v", away_team))
  
  # First Over Runs
  first_over_runs_overs <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "First Over Runs \\(1st Inn\\)")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "First Over Runs (First Innings)",
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  first_over_runs_unders <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "First Over Runs \\(1st Inn\\)")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "First Over Runs (First Innings)",
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  first_over_runs <-
    first_over_runs_overs |>
    left_join(first_over_runs_unders,
              by = c("match", "market", "home_team", "away_team", "line")) |>
    mutate(agency = "Sportsbet") |> 
    select(-home_team, -away_team)
  
  # Method of first dismissal
  first_dismissal_data <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "Method of First Dismissal \\(1st Inn\\)")) |>
    transmute(
      match,
      market = "Method of 1st Dismissal (1st Innings)",
      home_team,
      away_team,
      method = selection_name_prop,
      price = prop_market_price
    ) |>
    mutate(agency = "Sportsbet")
  
  # Runs at fall of first wicket
  first_wicket_runs_overs <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "Runs at Fall of 1st Wicket \\(1st Inn\\)")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "Runs at Fall of 1st Wicket (First Innings)",
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  first_wicket_runs_unders <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "Runs at Fall of 1st Wicket \\(1st Inn\\)")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "Runs at Fall of 1st Wicket (First Innings)",
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  first_wicket_runs <-
    first_wicket_runs_overs |>
    left_join(
      first_wicket_runs_unders,
      by = c("match", "market", "home_team", "away_team", "line")
    ) |>
    mutate(agency = "Sportsbet") |> 
    select(-home_team, -away_team)
  
  # First Innings Total
  first_innings_total_overs <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "First Innings Total")) |>
    filter(str_detect(selection_name_prop, "Over")) |>
    transmute(
      match,
      market = "First Innings Total",
      home_team,
      away_team,
      line = handicap,
      over_price = prop_market_price
    )
  
  first_innings_total_unders <-
    first_innings_data |>
    filter(str_detect(prop_market_name, "First Innings Total")) |>
    filter(str_detect(selection_name_prop, "Under")) |>
    transmute(
      match,
      market = "First Innings Total",
      home_team,
      away_team,
      line = handicap,
      under_price = prop_market_price
    )
  
  first_innings_total <-
    first_innings_total_overs |>
    left_join(
      first_innings_total_unders,
      by = c("match", "market", "home_team", "away_team", "line")
    ) |>
    mutate(agency = "Sportsbet")
  
  # Write to csv----------------------------------------------------------------
  write_csv(first_over_runs,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_first_over_runs_first_innings.csv")
  write_csv(first_dismissal_data,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_first_dismissal_first_innings.csv")
  write_csv(first_wicket_runs,
            "Data/T20s/Major League Cricket/scraped_odds/sportsbet_runs_at_first_wicket_first_innings.csv")
  
  #=============================================================================
  # Boundaries
  #=============================================================================
  
  # # Map function to player boundaries urls
  # player_boundaries_data <-
  #   map(player_boundaries_links, safe_read_prop_url)
  # 
  # # Get just result part from output
  # player_boundaries_data <-
  #   player_boundaries_data |>
  #   map("result") |>
  #   map_df(bind_rows) |>
  #   mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
  #   rename(match_id = url) |>
  #   mutate(match_id = as.numeric(match_id)) |>
  #   left_join(team_names, by = "match_id") |>
  #   mutate(match = paste(home_team, "v", away_team))
  # 
  # # Over Under boundaries
  # player_boundaries_overs <-
  #   player_boundaries_data |>
  #   filter(str_detect(prop_market_name, "Number of")) |>
  #   filter(str_detect(selection_name_prop, "Over")) |>
  #   transmute(
  #     match,
  #     market = str_extract(prop_market_name, "Number of .*"),
  #     home_team,
  #     away_team,
  #     player_name = str_remove(selection_name_prop, " - Over"),
  #     line = handicap,
  #     over_price = prop_market_price
  #   ) |>
  #   mutate(
  #     player_name = case_when(
  #       player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
  #       player_name == "Tom Rogers." ~ "Tom Rogers",
  #       player_name == "Matt Renshaw" ~ "Matthew Renshaw",
  #       player_name == "Oliver Davies" ~ "Ollie Davies",
  #       .default = player_name
  #     )
  #   )
  # 
  # player_boundaries_unders <-
  #   player_boundaries_data |>
  #   filter(str_detect(prop_market_name, "Number of")) |>
  #   filter(str_detect(selection_name_prop, "Under")) |>
  #   transmute(
  #     match,
  #     market = str_extract(prop_market_name, "Number of .*"),
  #     home_team,
  #     away_team,
  #     player_name = str_remove(selection_name_prop, " - Under"),
  #     line = handicap,
  #     under_price = prop_market_price
  #   ) |>
  #   mutate(
  #     player_name = case_when(
  #       player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
  #       player_name == "Tom Rogers." ~ "Tom Rogers",
  #       player_name == "Matt Renshaw" ~ "Matthew Renshaw",
  #       player_name == "Oliver Davies" ~ "Ollie Davies",
  #       .default = player_name
  #     )
  #   )
  # 
  # player_boundaries_combined <-
  #   player_boundaries_overs |>
  #   left_join(
  #     player_boundaries_unders,
  #     by = c(
  #       "match",
  #       "player_name",
  #       "market",
  #       "home_team",
  #       "away_team",
  #       "line"
  #     )
  #   ) |>
  #   mutate(agency = "Sportsbet") |>
  #   mutate(
  #     player_name = case_when(
  #       player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
  #       player_name == "Tom Rogers." ~ "Tom Rogers",
  #       player_name == "Matt Renshaw" ~ "Matthew Renshaw",
  #       player_name == "Oliver Davies" ~ "Ollie Davies",
  #       .default = player_name
  #     )
  #   ) |>
  #   left_join(player_teams[, c("player_name", "player_team")], by = "player_name") |>
  #   mutate(
  #     opposition_team = case_when(
  #       player_team == home_team ~ away_team,
  #       player_team == away_team ~ home_team
  #     )
  #   ) |>
  #   relocate(player_team, opposition_team, .after = player_name)
  # 
  # # Get Player to score a 6 markets
  # to_score_a_6 <-
  #   top_run_scorers_data |>
  #   filter(str_detect(prop_market_name, "Player To Hit A Six")) |>
  #   separate(
  #     match,
  #     into = c("home_team", "away_team"),
  #     sep = " v ",
  #     remove = FALSE
  #   ) |>
  #   transmute(
  #     match,
  #     market = "Number of 6s",
  #     home_team,
  #     away_team,
  #     player_name = selection_name_prop,
  #     line = 0.5,
  #     over_price = prop_market_price
  #   ) |>
  #   mutate(
  #     player_name = case_when(
  #       player_name == "Tom Rogers (Stars)" ~ "Tom F Rogers",
  #       player_name == "Tom Rogers." ~ "Tom Rogers",
  #       player_name == "Matt Renshaw" ~ "Matthew Renshaw",
  #       player_name == "Oliver Davies" ~ "Ollie Davies",
  #       .default = player_name
  #     )
  #   ) |>
  #   left_join(player_teams[, c("player_name", "player_team")], by = "player_name") |>
  #   mutate(
  #     opposition_team = case_when(
  #       player_team == home_team ~ away_team,
  #       player_team == away_team ~ home_team
  #     )
  #   ) |>
  #   relocate(player_team, opposition_team, .after = player_name) |>
  #   mutate(agency = "Sportsbet")
  # 
  # # Write to csv----------------------------------------------------------------
  # player_boundaries_combined |>
  #   bind_rows(to_score_a_6) |>
  #   write_csv("Data/T20s/Major League Cricket/scraped_odds/sportsbet_player_boundaries.csv")
}

##%######################################################%##
#                                                          #
####                Run functions safely                ####
#                                                          #
##%######################################################%##

safe_main_markets <- safely(main_markets_function, otherwise = NULL)
safe_player_props <- safely(player_props_function, otherwise = NULL)

safe_main_markets()
safe_player_props()
