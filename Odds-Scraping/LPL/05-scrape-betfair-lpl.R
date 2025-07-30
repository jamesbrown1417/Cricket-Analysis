#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(chromote)
library(rvest)

# Go to page
b <- ChromoteSession$new()
b$Page$navigate("https://www.betfair.com.au/exchange/plus/cricket/competition/12304154")

# Wait for page to load
Sys.sleep(1)

# View page (comment out when done)
b$view()

# Wait for page to load
Sys.sleep(1)

# evaluate js in Chromeote and work with returned string
markets_list <-
b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>% 
  read_html() %>%
  # Get a node with class = "mod-link"
  html_nodes(".mod-link") %>%
  # Filter to only those where data-link-type="MARKET"
  keep(~html_attr(., "data-link-type") == "MARKET")

# For the lpl elements get the href text
lpl_elements_href <-
markets_list %>%
  html_attr("href")

# Append the base url to the href
base_ref = "https://www.betfair.com.au/exchange/plus/"
lpl_elements_href <- paste0(base_ref, lpl_elements_href)

# Loop over list of lpl elements, go to the page and scrape the odds
for (i in 1:length(lpl_elements_href)) {
  # Go to page
  b$Page$navigate(lpl_elements_href[i])
  
  # Wait for page to load
  Sys.sleep(1)
  
  # View page (comment out when done)
  b$view()
  
  # Wait for page to load
  Sys.sleep(1)
  
  # Get HTML - back
  back_buttons <-
  b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>% 
    read_html() %>%
    # Get a node with type = "back"
    html_nodes(".back")
  
  # Get the back table
  back_table <-
  back_buttons %>%
    html_text() %>%
    # Remove all new line and whitespace
    str_remove_all("\\s+") %>%
    # Separate string
    str_split("\\$") %>%
    # Turn into tibble
    as.data.frame() %>%
    # Transpose
    t() %>%
    as_tibble() |> 
    transmute(back_odds = as.numeric(V1), back_liquidity = as.numeric(V2))
  
  # Get the HTML - lay
  lay_buttons <-
  b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>% 
    read_html() %>%
    # Get a node with type = "lay"
    html_nodes(".lay")
  
  # Get the lay table
  lay_table <-
  lay_buttons %>%
    html_text() %>%
    # Remove all new line and whitespace
    str_remove_all("\\s+") %>%
    # Separate string
    str_split("\\$") %>%
    # Turn into tibble
    as.data.frame() %>%
    # Transpose
    t() %>%
    as_tibble() |>
    transmute(lay_odds = as.numeric(V1), lay_liquidity = as.numeric(V2))
    
    # Get title
    title <-
    b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>% 
      read_html() %>%
      html_nodes(".event-header") %>%
      html_text() |> 
      str_remove_all(" SUBNAV.*")
    
    # Get selection
    selections <-
    b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value %>% 
      read_html() %>%
      html_nodes(".runner-name") %>%
      html_text()
    
    new_selections <- c()
    
    # for length of back table, repeat first element of selections half the time and second the rest
    for (i in 1:nrow(back_table)) {
      if (i <= nrow(back_table)/2) {
        new_selections <- c(new_selections, selections[1])
      } else {
        new_selections <- c(new_selections, selections[2])
      }
    }
    
    # Create output tibble
    output <- bind_cols(back_table, lay_table) |>
      mutate(match = title, selection = new_selections)
}
