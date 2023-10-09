##%######################################################%##
#                                                          #
####                       Set up                       ####
#                                                          #
##%######################################################%##

# Libraries---------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggtext)
library(readxl)

# Data--------------------------------------------------------------------------
test_innings_stats <-
  read_rds("test-innings-stats-batsmen.rds") |> 
  ungroup() |> 
  filter(!is.na(Runs))

# Add grounds to data
grounds <- read_excel("grounds.xlsx")

test_innings_stats <-
test_innings_stats |>
  left_join(grounds)

# Create function to filter and display player data
display_player_stats <-
  function(player_name,
           n_games = NULL,
           opposition = "all",
           ground = "all") {
    displayed_data <-
      test_innings_stats |>
      filter(Player == player_name)
    
    if (opposition != "all") {
      displayed_data <-
        displayed_data |>
        filter(Opposition == opposition)
    }
    
    if (ground != "all") {
      displayed_data <-
        displayed_data |>
        filter(Ground == ground)
    }
    
    if (!is.null(n_games)) {
      displayed_data <-
        displayed_data |>
        slice_tail(n = n_games)
    }
    
    return(displayed_data)
    
  }

# Function to plot stats
display_data <-
  function(data) {
   
    # Remove innings with no batting
    data <-
    data |> 
      arrange(desc(Date)) |> 
      mutate(match_num = row_number())
    
    # Get summary stats
    n_games <- nrow(data)
    player_name <- data$Player[1]
    dismissals <- sum(!data$NotOut)
    avg_runs <- (sum(data$Runs, na.rm = TRUE) / dismissals) |> round(2)
    highest_score <- max(data$Runs, na.rm = TRUE)
    fifties <- sum(data$Runs >= 50)
    hundreds <- sum(data$Runs >= 100)
    under_10 <- sum(data$Runs < 10 & !data$NotOut)
    
    # Plot data
    data |> 
      ggplot(aes(x = match_num, y = Runs)) +
      geom_point(color = "orangered2", size = 2.5) +
      geom_path(color = "black") +
      labs(title = paste(player_name, "Last", n_games, "Innings"),
           x = "Match Number",
           y = "Runs",
           caption = paste0("**Average:** ", avg_runs,
                            "<br>", "**Highest Score:** ",
                            highest_score, "<br>",
                            "**Fifties:** ", fifties, "<br>",
                            "**Hundreds:** ", hundreds)) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        plot.caption = element_markdown(hjust = 0.5)
      )
  }

# Function to summarise player performance by opposition
player_performance_opposition <- function(player_name) {
test_innings_stats |>
  filter(Player == player_name) |> 
  filter(!is.na(Runs)) |> 
  group_by(Opposition) |> 
  summarise(innings = n(), total_runs = sum(Runs), total_dismissals = sum(!NotOut)) |> 
  mutate(avg = round(total_runs / total_dismissals, 3)) |> 
  arrange(desc(avg))
}


# Function to summarise player performance by home or away
player_performance_home_away <- function(player_name) {
  test_innings_stats |>
    filter(Player == player_name) |> 
    filter(!is.na(Runs)) |> 
    mutate(home_away = Location == Country) |>
    mutate(home_away = factor(home_away, levels = c(TRUE, FALSE), labels = c("Home", "Overseas"))) |> 
    group_by(home_away) |> 
    summarise(innings = n(), total_runs = sum(Runs), total_dismissals = sum(!NotOut)) |> 
    mutate(avg = round(total_runs / total_dismissals, 3)) |> 
    arrange(desc(avg)) |> 
    rename(location = home_away)
}

##%######################################################%##
#                                                          #
####                        App                         ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####                       UI                            ####
#                                                          #
##%######################################################%##
ui <- fluidPage(titlePanel("Player Performance Analysis"),
                theme = shinytheme("united"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("player_name", "Enter Player's Name"),
                    textInput("opposition", label = "Opposition Team", value = "all"),
                    textInput("ground", "Venue", value = "all"),
                    numericInput(
                      "n_games",
                      "Number of Innings to Display",
                      value = 5,
                      min = 1
                    ),
                    h3("Player Stats Plot"),
                    plotOutput("player_stats_plot", height = "400px")
                  ),
                  mainPanel(h3("Player Stats Table"),
                            dataTableOutput("player_stats_table"))
                )
)

##%######################################################%##
#                                                          #
####                   Server logic                     ####
#                                                          #
##%######################################################%##

server <- function(input, output) {
  
  output$player_stats_table <- renderDataTable({
    display_player_stats(input$player_name, input$n_games, opposition = input$opposition, ground = input$ground)
  })
  
  output$player_stats_plot <- renderPlot({
    data <- display_player_stats(input$player_name, input$n_games, opposition = input$opposition, ground = input$ground)
    display_data(data)
  })
  
}

##%######################################################%##
#                                                          #
####                       Run                          ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
