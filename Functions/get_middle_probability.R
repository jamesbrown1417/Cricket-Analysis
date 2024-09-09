calculate_middle_roi <- function(market_data, value_column, low_line_limit, high_line_limit, 
                                 low_line_price, high_line_price) {
  # Get all results in range (middle)
  results_middle <- 
    market_data |>
    filter({{ value_column }} >= low_line_limit & {{ value_column }} <= high_line_limit)
  
  # Get all results above the high line (under bet fails, over wins)
  results_above <- 
    market_data |>
    filter({{ value_column }} > high_line_limit)
  
  # Get all results below the low line (over bet fails, under wins)
  results_below <- 
    market_data |>
    filter({{ value_column }} < low_line_limit)
  
  # Tally up profit or loss for overs (low line)
  results_profit_over <- ((low_line_price * 100 - 100) - 100) * nrow(results_below)
  
  # Tally up profit or loss for unders (high line)
  results_profit_under <- ((high_line_price * 100 - 100) - 100) * nrow(results_above)
  
  # Tally up profit when the middle hits (both overs and unders win)
  results_profit_middle <- ((low_line_price * 100 - 100) + (high_line_price * 100 - 100)) * nrow(results_middle)
  
  # Get total profit or loss
  total_profit_loss <- results_profit_over + results_profit_under + results_profit_middle
  
  # Get total staked
  total_staked <- 200 * (nrow(results_below) + nrow(results_above) + nrow(results_middle))
  
  # Get the ROI
  roi <- 100 * (total_profit_loss / total_staked)
  
  # Return a tibble with the market and results, inferring low and high line from limits
  tibble(
    market = deparse(substitute(market_data)),
    low_line = paste("Over", low_line_limit),
    low_line_price = low_line_price,
    high_line = paste("Under", high_line_limit),
    high_line_price = high_line_price,
    profit = total_profit_loss,
    staked = total_staked,
    roi_percentage = roi
  )
}
