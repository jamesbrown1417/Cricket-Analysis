# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/Cricket/featured?homeState=SA&jurisdiction=SA"

  # Make request and get response
  tab_response <-
    request(tab_url) |>
    req_perform() |> 
    resp_body_json()
  
  # Function to extract market info from response---------------------------------
  get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
  }
  
  # Function to extract match info from response----------------------------------
  get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_round = matches$round
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
      match = match_name,
      round = match_round,
      start_time = match_start_time,
      market_name = market_info$market,
      propositions = market_info$propositions
    )
  }
  
  # Map functions to data
  all_tab_markets <-
    map(tab_response$competitions[[1]]$matches, get_match_info) |> bind_rows()
  
  # Expand list col into multiple cols
  all_tab_markets <-
    all_tab_markets |>
    unnest_wider(col = propositions, names_sep = "_") |>
    select(any_of(c("match",
                    "round",
                    "start_time",
                    "market_name")),
           prop_name = propositions_name,
           price = propositions_returnWin)
  
 