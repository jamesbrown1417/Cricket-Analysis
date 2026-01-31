# ========================================
# Centralized Sportsbet Scraping Function
# ========================================
# This function contains all the scraping logic for Sportsbet cricket odds.
# Each competition script just calls this function with competition-specific
# parameters (URL, output directory, team name fixer, etc.)

library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(glue)

scrape_sportsbet <- function(
    sportsbet_url,
    output_dir,
    competition_name = "Competition",
    fix_team_names = identity,
    fix_player_names = identity,
    use_live_html = TRUE,
    scrape_milestones = TRUE,
    scrape_extra_match_markets = TRUE
) {

  # Create output directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  message(glue("Starting Sportsbet scrape for {competition_name}..."))
  message(glue("URL: {sportsbet_url}"))
  message(glue("Output: {output_dir}"))

  #=============================================================================
  # Fetch HTML
  #=============================================================================

  message("Fetching Sportsbet HTML...")

  if (use_live_html) {
    sportsbet_html <- sportsbet_url |> read_html_live()
  } else {
    sportsbet_html <- sportsbet_url |> read_html()
  }

  #=============================================================================
  # Helper: Read a Sportsbet API URL and parse player props
  #=============================================================================

  read_prop_url <- function(url) {

    sb_response <-
      request(url) |>
      req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36") |>
      req_headers("Referer" = "https://www.sportsbet.com.au") |>
      req_perform() |>
      resp_body_json()

    prop_market_name <- c()
    selection_name_prop <- c()
    prop_market_selection <- c()
    prop_market_price <- c()
    player_id <- c()
    market_id <- c()
    handicap <- c()

    for (market in sb_response) {
      for (selection in market$selections) {

        prop_market_name <- c(prop_market_name, market$name)
        selection_name_prop <- c(selection_name_prop, selection$name)
        prop_market_selection <- c(prop_market_selection, selection$resultType)
        player_id <- c(player_id, selection$externalId)
        market_id <- c(market_id, market$externalId)

        if (is.null(selection$price$winPrice)) {
          prop_market_price <- c(prop_market_price, NA)
        } else {
          prop_market_price <- c(prop_market_price, as.numeric(selection$price$winPrice))
        }

        if (is.null(selection$unformattedHandicap)) {
          handicap <- c(handicap, NA)
        } else {
          handicap <- c(handicap, as.numeric(selection$unformattedHandicap))
        }
      }
    }

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

  safe_read_prop_url <- safely(read_prop_url, otherwise = NULL)

  #=============================================================================
  # Helper: Fetch API data for a set of links and join with team names
  #=============================================================================

  process_api_data <- function(api_links, team_names_df) {

    raw_data <- map(api_links, safe_read_prop_url)

    result <-
      raw_data |>
      map("result") |>
      map_df(bind_rows)

    if (nrow(result) == 0) {
      return(tibble(
        prop_market_name = character(),
        selection_name_prop = character(),
        prop_market_selection = character(),
        prop_market_price = numeric(),
        player_id = character(),
        market_id = character(),
        handicap = numeric(),
        url = character(),
        match_id = numeric(),
        home_team = character(),
        away_team = character(),
        match = character()
      ))
    }

    result |>
      mutate(url = str_extract(as.character(url), "[0-9]{6,8}")) |>
      rename(match_id = url) |>
      mutate(match_id = as.numeric(match_id)) |>
      left_join(team_names_df, by = "match_id") |>
      mutate(home_team = fix_team_names(home_team)) |>
      mutate(away_team = fix_team_names(away_team)) |>
      mutate(match = paste(home_team, "v", away_team))
  }

  #=============================================================================
  # Main Markets (H2H)
  #=============================================================================

  main_markets_function <- function() {

    message("Scraping main markets (H2H)...")

    matches <-
      sportsbet_html |>
      html_nodes(".White_fqa53j6")

    get_team_names <- function(match) {
      team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()

      home_team <- team_names[1]
      away_team <- team_names[2]

      tibble(home_team, away_team)
    }

    get_odds <- function(match) {
      odds <-
        match |>
        html_nodes(".priceTextSize_frw9zm9") |>
        html_text() |>
        as.numeric()

      home_win <- odds[1]
      away_win <- odds[2]

      tibble(home_win, away_win)
    }

    all_main_market_data <-
      bind_cols(
        map(matches, get_team_names) |> bind_rows() |> filter(!is.na(home_team)),
        map(matches, get_odds) |> bind_rows() |> filter(!is.na(home_win))
      )

    sportsbet_h2h <-
      all_main_market_data |>
      mutate(home_team = fix_team_names(home_team)) |>
      mutate(away_team = fix_team_names(away_team)) |>
      mutate(match = paste(home_team, "v", away_team)) |>
      mutate(market_name = "Head To Head") |>
      mutate(home_win = as.numeric(home_win)) |>
      mutate(away_win = as.numeric(away_win)) |>
      select(match, market_name, home_team, home_win, away_team, away_win) |>
      mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
      mutate(agency = "Sportsbet")

    write_csv(sportsbet_h2h, file.path(output_dir, "sportsbet_h2h.csv"))
    message(glue("  Wrote {nrow(sportsbet_h2h)} H2H rows"))
  }

  #=============================================================================
  # Player Props (API-based markets)
  #=============================================================================

  player_props_function <- function() {

    message("Scraping player props...")

    # Get team names from HTML
    get_team_names <- function(match) {
      team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()

      home_team <- team_names[1]
      away_team <- team_names[2]

      tibble(home_team, away_team)
    }

    # Get match links and IDs
    match_links <-
      sportsbet_html |>
      html_nodes(".linkMultiMarket_fcmecz0") |>
      html_attr("href")

    match_ids <-
      match_links |>
      str_extract("\\d{4,10}$") |>
      as.numeric()

    matches <-
      sportsbet_html |>
      html_nodes(".White_fqa53j6")

    team_names <-
      map_dfr(matches, get_team_names) |>
      filter(!is.na(home_team)) |>
      bind_cols("match_id" = match_ids)

    # Build API links for each market grouping
    run_scorer_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/306/Markets")
    top_wicket_takers_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/307/Markets")
    match_markets_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/326/Markets")
    team_totals_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/328/Markets")
    first_innings_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/325/Markets")
    x_over_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/658/Markets")
    fall_of_wicket_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/435/Markets")
    player_boundaries_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/429/Markets")

    #===========================================================================
    # Player Runs
    #===========================================================================

    message("  Scraping player runs...")

    player_runs_data <- process_api_data(run_scorer_links, team_names)

    # Top match run scorer (handles both "Runscorer" and "Batter" naming)
    match_top_run_scorer <-
      player_runs_data |>
      filter(str_detect(prop_market_name, "Top Match (Runscorer|Batter)")) |>
      transmute(
        match,
        market = "Top Match Run Scorer",
        home_team,
        away_team,
        player_name = selection_name_prop,
        price = prop_market_price
      ) |>
      mutate(player_name = fix_player_names(player_name))

    # Player runs over/under
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
        by = c("match", "market", "home_team", "away_team", "player_name", "line")
      ) |>
      mutate(player_name = str_remove(player_name, " Total Runs.*$")) |>
      mutate(player_name = str_remove(player_name, " Alternate.*$")) |>
      mutate(player_name = fix_player_names(player_name)) |>
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
      mutate(player_name = fix_player_names(player_name)) |>
      mutate(agency = "Sportsbet")

    player_runs_all <-
      bind_rows(player_runs_combined, player_runs_alternative_lines) |>
      arrange(player_name, line)

    write_csv(match_top_run_scorer, file.path(output_dir, "sportsbet_match_top_run_scorer.csv"))
    write_csv(player_runs_all, file.path(output_dir, "sportsbet_player_runs.csv"))
    message(glue("  Wrote {nrow(match_top_run_scorer)} top run scorer rows"))
    message(glue("  Wrote {nrow(player_runs_all)} player runs rows"))

    #===========================================================================
    # Player Milestones (optional - e.g. IPL)
    #===========================================================================

    if (scrape_milestones) {

      message("  Scraping player milestones...")

      milestones_links <- glue("https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/{match_ids}/MarketGroupings/903/Markets")

      player_milestones_data <- process_api_data(milestones_links, team_names)

      if (nrow(player_milestones_data) > 0) {
        # To score 50 - Yes
        player_fifties <-
          player_milestones_data |>
          filter(str_detect(prop_market_name, "50")) |>
          filter(selection_name_prop == "Yes") |>
          transmute(
            match,
            market = "Player Fifties",
            home_team,
            away_team,
            player_name = str_remove(prop_market_name, " to Reach 50.*$"),
            yes_price = prop_market_price
          ) |>
          mutate(player_name = fix_player_names(player_name)) |>
          mutate(agency = "Sportsbet")

        # To score 50 - No
        player_fifties_no <-
          player_milestones_data |>
          filter(str_detect(prop_market_name, "50")) |>
          filter(selection_name_prop == "No") |>
          transmute(
            match,
            market = "Player Fifties",
            home_team,
            away_team,
            player_name = str_remove(prop_market_name, " to Reach 50.*$"),
            no_price = prop_market_price
          ) |>
          mutate(player_name = fix_player_names(player_name)) |>
          mutate(agency = "Sportsbet")

        player_fifties_combined <-
          player_fifties |>
          inner_join(player_fifties_no) |>
          relocate(no_price, .after = yes_price)

        write_csv(player_fifties_combined, file.path(output_dir, "sportsbet_player_fifties.csv"))
        message(glue("  Wrote {nrow(player_fifties_combined)} player fifties rows"))
      }
    }

    #===========================================================================
    # Top Wicket Takers
    #===========================================================================

    message("  Scraping wicket takers...")

    top_wicket_takers_data <- process_api_data(top_wicket_takers_links, team_names)

    # Home team top wicket taker
    home_top_wicket_taker <-
      top_wicket_takers_data |>
      filter(str_detect(prop_market_name, "Top .* Wicket Taker")) |>
      mutate(player_team = str_remove(prop_market_name, "Top ")) |>
      mutate(player_team = str_remove(player_team, " Wicket Taker.*$")) |>
      mutate(player_team = fix_team_names(player_team)) |>
      filter(player_team == home_team) |>
      transmute(
        match,
        market = "Top Team Wicket Taker",
        home_team,
        away_team,
        player_name = selection_name_prop,
        player_team,
        opposition_team = away_team,
        price = prop_market_price,
        agency = "Sportsbet"
      )

    # Away team top wicket taker
    away_top_wicket_taker <-
      top_wicket_takers_data |>
      filter(str_detect(prop_market_name, "Top .* Wicket Taker")) |>
      mutate(player_team = str_remove(prop_market_name, "Top ")) |>
      mutate(player_team = str_remove(player_team, " Wicket Taker.*$")) |>
      mutate(player_team = fix_team_names(player_team)) |>
      filter(player_team == away_team) |>
      transmute(
        match,
        market = "Top Team Wicket Taker",
        home_team,
        away_team,
        player_name = selection_name_prop,
        player_team,
        opposition_team = home_team,
        price = prop_market_price,
        agency = "Sportsbet"
      )

    top_team_wicket_taker <-
      bind_rows(home_top_wicket_taker, away_top_wicket_taker) |>
      mutate(player_name = fix_player_names(player_name)) |>
      filter(!is.na(price))

    # Alternate player wickets
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
      mutate(player_name = fix_player_names(player_name)) |>
      mutate(agency = "Sportsbet")

    write_csv(player_wickets_alternative_lines, file.path(output_dir, "sportsbet_player_wickets.csv"))
    write_csv(top_team_wicket_taker, file.path(output_dir, "sportsbet_top_team_wicket_taker.csv"))
    message(glue("  Wrote {nrow(player_wickets_alternative_lines)} player wickets rows"))
    message(glue("  Wrote {nrow(top_team_wicket_taker)} top team wicket taker rows"))

    #===========================================================================
    # Team Totals (6 overs, 10 overs)
    #===========================================================================

    message("  Scraping team totals...")

    team_totals <- process_api_data(team_totals_links, team_names)

    # Six Over Total Runs
    team_totals_six_overs <-
      team_totals |>
      filter(str_detect(prop_market_name, "6 Overs")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      mutate(team = str_remove(prop_market_name, " 6 Overs.*")) |>
      mutate(team = fix_team_names(team)) |>
      transmute(
        match, market = "Team X Over Total Runs",
        home_team, away_team, team, overs = 6,
        line = handicap, over_price = prop_market_price
      )

    team_totals_six_unders <-
      team_totals |>
      filter(str_detect(prop_market_name, "6 Overs")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      mutate(team = str_remove(prop_market_name, " 6 Overs.*")) |>
      mutate(team = fix_team_names(team)) |>
      transmute(
        match, market = "Team X Over Total Runs",
        home_team, away_team, team, overs = 6,
        line = handicap, under_price = prop_market_price
      )

    team_totals_six <-
      team_totals_six_overs |>
      left_join(team_totals_six_unders) |>
      mutate(agency = "Sportsbet")

    # Ten Over Total Runs
    team_totals_ten_overs <-
      team_totals |>
      filter(str_detect(prop_market_name, "10 Overs")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      mutate(team = str_remove(prop_market_name, " 10 Overs.*")) |>
      mutate(team = fix_team_names(team)) |>
      transmute(
        match, market = "Team X Over Total Runs",
        home_team, away_team, team, overs = 10,
        line = handicap, over_price = prop_market_price
      )

    team_totals_ten_unders <-
      team_totals |>
      filter(str_detect(prop_market_name, "10 Overs")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      mutate(team = str_remove(prop_market_name, " 10 Overs.*")) |>
      mutate(team = fix_team_names(team)) |>
      transmute(
        match, market = "Team X Over Total Runs",
        home_team, away_team, team, overs = 10,
        line = handicap, under_price = prop_market_price
      )

    team_totals_ten <-
      team_totals_ten_overs |>
      left_join(team_totals_ten_unders) |>
      mutate(agency = "Sportsbet")

    write_csv(team_totals_six, file.path(output_dir, "sportsbet_team_totals_six_overs.csv"))
    write_csv(team_totals_ten, file.path(output_dir, "sportsbet_team_totals_ten_overs.csv"))
    message(glue("  Wrote {nrow(team_totals_six)} team totals 6 overs rows"))
    message(glue("  Wrote {nrow(team_totals_ten)} team totals 10 overs rows"))

    #===========================================================================
    # Match Markets (fours, sixes, team totals)
    #===========================================================================

    message("  Scraping match markets...")

    match_markets <- process_api_data(match_markets_links, team_names)

    # Match Fours
    match_fours_overs <-
      match_markets |>
      filter(str_detect(prop_market_name, "^Total Match Fours")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "Total Match Fours", home_team, away_team,
                line = handicap, over_price = prop_market_price)

    match_fours_unders <-
      match_markets |>
      filter(str_detect(prop_market_name, "^Total Match Fours")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "Total Match Fours", home_team, away_team,
                line = handicap, under_price = prop_market_price)

    match_fours <-
      match_fours_overs |>
      left_join(match_fours_unders, by = c("match", "home_team", "away_team", "line", "market")) |>
      mutate(agency = "Sportsbet")

    # Match Sixes
    match_sixes_overs <-
      match_markets |>
      filter(str_detect(prop_market_name, "^Total Match Sixes")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "Total Match Sixes", home_team, away_team,
                line = handicap, over_price = prop_market_price)

    match_sixes_unders <-
      match_markets |>
      filter(str_detect(prop_market_name, "^Total Match Sixes")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "Total Match Sixes", home_team, away_team,
                line = handicap, under_price = prop_market_price)

    match_sixes <-
      match_sixes_overs |>
      left_join(match_sixes_unders, by = c("match", "home_team", "away_team", "line", "market")) |>
      mutate(agency = "Sportsbet")

    # Team Sixes (flexible regex handles both "Total Match Sixes" and "Total Sixes")
    team_sixes_overs <-
      match_markets |>
      filter(str_detect(prop_market_name, ".+ Total .*Sixes")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "Total Team Sixes", home_team, away_team,
                team = str_remove(prop_market_name, " Total .*Sixes.*"),
                line = handicap, over_price = prop_market_price)

    team_sixes_unders <-
      match_markets |>
      filter(str_detect(prop_market_name, ".+ Total .*Sixes")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "Total Team Sixes", home_team, away_team,
                team = str_remove(prop_market_name, " Total .*Sixes.*"),
                line = handicap, under_price = prop_market_price)

    team_sixes <-
      team_sixes_overs |>
      left_join(team_sixes_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
      mutate(agency = "Sportsbet") |>
      mutate(team = fix_team_names(team))

    # Team Fours (flexible regex handles both "Total Match Fours" and "Total Fours")
    team_fours_overs <-
      match_markets |>
      filter(str_detect(prop_market_name, ".+ Total .*Fours")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "Total Team Fours", home_team, away_team,
                team = str_remove(prop_market_name, " Total .*Fours.*"),
                line = handicap, over_price = prop_market_price)

    team_fours_unders <-
      match_markets |>
      filter(str_detect(prop_market_name, ".+ Total .*Fours")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "Total Team Fours", home_team, away_team,
                team = str_remove(prop_market_name, " Total .*Fours.*"),
                line = handicap, under_price = prop_market_price)

    team_fours <-
      team_fours_overs |>
      left_join(team_fours_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
      mutate(agency = "Sportsbet") |>
      mutate(team = fix_team_names(team))

    # Team Total Runs (To Score X Runs)
    team_totals_overs <-
      match_markets |>
      filter(str_detect(prop_market_name, "To Score Runs")) |>
      transmute(
        match, market = "Total Team Runs", home_team, away_team,
        team = str_remove(prop_market_name, " To Score Runs"),
        line = as.numeric(str_extract(selection_name_prop, "[0-9]{1,3}")) - 0.5,
        over_price = prop_market_price,
        agency = "Sportsbet"
      )

    write_csv(team_totals_overs, file.path(output_dir, "sportsbet_team_totals_overs.csv"))
    write_csv(team_fours, file.path(output_dir, "sportsbet_team_fours.csv"))
    write_csv(team_sixes, file.path(output_dir, "sportsbet_team_sixes.csv"))
    write_csv(match_sixes, file.path(output_dir, "sportsbet_match_sixes.csv"))
    write_csv(match_fours, file.path(output_dir, "sportsbet_match_fours.csv"))
    message(glue("  Wrote match markets data"))

    # Extra match markets (e.g. MLC)
    if (scrape_extra_match_markets) {

      message("  Scraping extra match markets...")

      # Highest Opening Partnership
      highest_opening_partnership <-
        match_markets |>
        filter(str_detect(prop_market_name, "Highest Opening Partnership")) |>
        transmute(match, market = "Highest Opening Partnership",
                  home_team, away_team,
                  team = fix_team_names(selection_name_prop),
                  price = prop_market_price, agency = "Sportsbet")

      # Team To Score Most Fours
      team_to_score_most_fours <-
        match_markets |>
        filter(str_detect(prop_market_name, "Team To Score The Most Fours")) |>
        transmute(match, market = "Team To Score Most Fours",
                  home_team, away_team,
                  team = fix_team_names(selection_name_prop),
                  price = prop_market_price, agency = "Sportsbet")

      # Team To Score Most Sixes
      team_to_score_most_sixes <-
        match_markets |>
        filter(str_detect(prop_market_name, "Team To Score The Most Sixes")) |>
        transmute(match, market = "Team To Score Most Sixes",
                  home_team, away_team,
                  team = fix_team_names(selection_name_prop),
                  price = prop_market_price, agency = "Sportsbet")

      # Team of Top Batter
      team_of_top_batter <-
        match_markets |>
        filter(str_detect(prop_market_name, "Team of Top Batter")) |>
        transmute(match, market = "Team of Top Batter",
                  home_team, away_team,
                  team = fix_team_names(selection_name_prop),
                  price = prop_market_price, agency = "Sportsbet")

      # Total Match Run Outs
      total_match_run_outs_overs <-
        match_markets |>
        filter(str_detect(prop_market_name, "Total Match Run Outs")) |>
        filter(str_detect(selection_name_prop, "Over")) |>
        transmute(match, market = "Total Match Run Outs",
                  home_team, away_team,
                  line = handicap, over_price = prop_market_price)

      total_match_run_outs_unders <-
        match_markets |>
        filter(str_detect(prop_market_name, "Total Match Run Outs")) |>
        filter(str_detect(selection_name_prop, "Under")) |>
        transmute(match, market = "Total Match Run Outs",
                  home_team, away_team,
                  line = handicap, under_price = prop_market_price)

      total_match_run_outs <-
        total_match_run_outs_overs |>
        left_join(total_match_run_outs_unders,
                  by = c("match", "home_team", "away_team", "line", "market")) |>
        mutate(agency = "Sportsbet")

      write_csv(highest_opening_partnership, file.path(output_dir, "sportsbet_highest_opening_partnership.csv"))
      write_csv(team_to_score_most_fours, file.path(output_dir, "sportsbet_team_to_score_most_fours.csv"))
      write_csv(team_to_score_most_sixes, file.path(output_dir, "sportsbet_team_to_score_most_sixes.csv"))
      write_csv(team_of_top_batter, file.path(output_dir, "sportsbet_team_of_top_batter.csv"))
      write_csv(total_match_run_outs, file.path(output_dir, "sportsbet_total_match_run_outs.csv"))
      message(glue("  Wrote extra match markets data"))
    }

    #===========================================================================
    # X Overs (First Over Runs by Team)
    #===========================================================================

    message("  Scraping X overs data...")

    x_over_markets <- process_api_data(x_over_links, team_names)

    # First Over Runs
    first_over_runs_overs <-
      x_over_markets |>
      filter(str_detect(prop_market_name, "1st Over Total")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "First Over Runs - Team",
                team = str_remove(prop_market_name, " 1st Over Total .*"),
                home_team, away_team,
                line = handicap, over_price = prop_market_price)

    first_over_runs_unders <-
      x_over_markets |>
      filter(str_detect(prop_market_name, "1st Over Total")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "First Over Runs - Team",
                team = str_remove(prop_market_name, " 1st Over Total .*"),
                home_team, away_team,
                line = handicap, under_price = prop_market_price)

    first_over_runs_all <-
      first_over_runs_overs |>
      left_join(first_over_runs_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
      mutate(agency = "Sportsbet") |>
      select(-home_team, -away_team) |>
      mutate(team = fix_team_names(team))

    write_csv(first_over_runs_all, file.path(output_dir, "sportsbet_first_over_runs.csv"))
    message(glue("  Wrote {nrow(first_over_runs_all)} first over runs rows"))

    #===========================================================================
    # Fall of Wicket
    #===========================================================================

    message("  Scraping fall of wicket data...")

    fall_of_wicket_markets <- process_api_data(fall_of_wicket_links, team_names)

    fall_of_wicket_overs <-
      fall_of_wicket_markets |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "Fall of 1st Wicket - Team",
                team = str_remove(prop_market_name, " Runs at Fall of .*"),
                home_team, away_team,
                line = handicap, over_price = prop_market_price)

    fall_of_wicket_unders <-
      fall_of_wicket_markets |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "Fall of 1st Wicket - Team",
                team = str_remove(prop_market_name, " Runs at Fall of .*"),
                home_team, away_team,
                line = handicap, under_price = prop_market_price)

    fall_of_wicket_all <-
      fall_of_wicket_overs |>
      left_join(fall_of_wicket_unders, by = c("match", "home_team", "away_team", "team", "line", "market")) |>
      mutate(agency = "Sportsbet") |>
      mutate(team = fix_team_names(team))

    write_csv(fall_of_wicket_all, file.path(output_dir, "sportsbet_runs_at_first_wicket.csv"))
    message(glue("  Wrote {nrow(fall_of_wicket_all)} fall of wicket rows"))

    #===========================================================================
    # First Innings
    #===========================================================================

    message("  Scraping first innings data...")

    first_innings_data <- process_api_data(first_innings_links, team_names)

    # First Over Runs (First Innings)
    fi_first_over_overs <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "First Over Runs \\(1st Inn\\)")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "First Over Runs (First Innings)",
                home_team, away_team,
                line = handicap, over_price = prop_market_price)

    fi_first_over_unders <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "First Over Runs \\(1st Inn\\)")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "First Over Runs (First Innings)",
                home_team, away_team,
                line = handicap, under_price = prop_market_price)

    first_over_runs <-
      fi_first_over_overs |>
      left_join(fi_first_over_unders, by = c("match", "market", "home_team", "away_team", "line")) |>
      mutate(agency = "Sportsbet") |>
      select(-home_team, -away_team)

    # Method of first dismissal
    first_dismissal_data <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "Method of First Dismissal \\(1st Inn\\)")) |>
      transmute(match, market = "Method of 1st Dismissal (1st Innings)",
                home_team, away_team,
                method = selection_name_prop,
                price = prop_market_price) |>
      mutate(agency = "Sportsbet")

    # Runs at fall of first wicket (first innings)
    fi_wicket_overs <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "Runs at Fall of 1st Wicket \\(1st Inn\\)")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "Runs at Fall of 1st Wicket (First Innings)",
                home_team, away_team,
                line = handicap, over_price = prop_market_price)

    fi_wicket_unders <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "Runs at Fall of 1st Wicket \\(1st Inn\\)")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "Runs at Fall of 1st Wicket (First Innings)",
                home_team, away_team,
                line = handicap, under_price = prop_market_price)

    first_wicket_runs <-
      fi_wicket_overs |>
      left_join(fi_wicket_unders, by = c("match", "market", "home_team", "away_team", "line")) |>
      mutate(agency = "Sportsbet") |>
      select(-home_team, -away_team)

    # First Innings Total
    fi_total_overs <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "First Innings Total")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = "First Innings Total", home_team, away_team,
                line = handicap, over_price = prop_market_price)

    fi_total_unders <-
      first_innings_data |>
      filter(str_detect(prop_market_name, "First Innings Total")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = "First Innings Total", home_team, away_team,
                line = handicap, under_price = prop_market_price)

    first_innings_total <-
      fi_total_overs |>
      left_join(fi_total_unders, by = c("match", "market", "home_team", "away_team", "line")) |>
      mutate(agency = "Sportsbet")

    write_csv(first_over_runs, file.path(output_dir, "sportsbet_first_over_runs_first_innings.csv"))
    write_csv(first_dismissal_data, file.path(output_dir, "sportsbet_first_dismissal_first_innings.csv"))
    write_csv(first_wicket_runs, file.path(output_dir, "sportsbet_runs_at_first_wicket_first_innings.csv"))
    write_csv(first_innings_total, file.path(output_dir, "sportsbet_first_innings_total.csv"))
    message(glue("  Wrote first innings data"))

    #===========================================================================
    # Player Boundaries
    #===========================================================================

    message("  Scraping player boundaries...")

    player_boundaries_data <- process_api_data(player_boundaries_links, team_names)

    # Over/Under boundaries
    player_boundaries_overs <-
      player_boundaries_data |>
      filter(str_detect(prop_market_name, "Number of")) |>
      filter(str_detect(selection_name_prop, "Over")) |>
      transmute(match, market = str_extract(prop_market_name, "Number of .*"),
                home_team, away_team,
                player_name = str_remove(selection_name_prop, " - Over"),
                line = handicap, over_price = prop_market_price) |>
      mutate(player_name = fix_player_names(player_name))

    player_boundaries_unders <-
      player_boundaries_data |>
      filter(str_detect(prop_market_name, "Number of")) |>
      filter(str_detect(selection_name_prop, "Under")) |>
      transmute(match, market = str_extract(prop_market_name, "Number of .*"),
                home_team, away_team,
                player_name = str_remove(selection_name_prop, " - Under"),
                line = handicap, under_price = prop_market_price) |>
      mutate(player_name = fix_player_names(player_name))

    player_boundaries_combined <-
      player_boundaries_overs |>
      left_join(
        player_boundaries_unders,
        by = c("match", "player_name", "market", "home_team", "away_team", "line")
      ) |>
      mutate(agency = "Sportsbet") |>
      mutate(player_name = fix_player_names(player_name))

    # Player to hit a six
    to_score_a_6 <-
      player_boundaries_data |>
      filter(str_detect(prop_market_name, "Player To Hit A Six")) |>
      separate(
        match, into = c("home_team", "away_team"),
        sep = " v ", remove = FALSE
      ) |>
      transmute(match, market = "Number of 6s", home_team, away_team,
                player_name = selection_name_prop,
                line = 0.5,
                over_price = prop_market_price) |>
      mutate(player_name = fix_player_names(player_name)) |>
      mutate(agency = "Sportsbet")

    player_boundaries_combined |>
      bind_rows(to_score_a_6) |>
      write_csv(file.path(output_dir, "sportsbet_player_boundaries.csv"))

    message(glue("  Wrote player boundaries data"))
  }

  #=============================================================================
  # Run both functions with error handling
  #=============================================================================

  safe_main_markets <- safely(main_markets_function, otherwise = NULL)
  safe_player_props <- safely(player_props_function, otherwise = NULL)

  h2h_result <- safe_main_markets()
  if (!is.null(h2h_result$error)) {
    message(glue("Warning: H2H scraping failed - {h2h_result$error$message}"))
  }

  props_result <- safe_player_props()
  if (!is.null(props_result$error)) {
    message(glue("Warning: Player props scraping failed - {props_result$error$message}"))
  }

  message(glue("Sportsbet scrape for {competition_name} complete!"))
}
