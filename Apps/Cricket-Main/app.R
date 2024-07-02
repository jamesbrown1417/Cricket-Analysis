library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)
library(googlesheets4)
library(googledrive)

#===============================================================================
# Load in data
#===============================================================================

# Read in Innings Stats
innings_stats <-
  read_rds("../../Data/T20s/Major League Cricket/t20_mlc_match_innings_data.rds") |> 
  bind_rows(read_rds("../../Data/T20s/CPL/cpl_match_innings_data.rds")) |> 
  bind_rows(read_rds("../../Data/T20s/Big Bash/bbl_match_innings_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/IPL/ipl_match_innings_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/The Hundred/the_hundred_match_innings_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/Internationals/t20_international_match_innings_data.rds"))

# Read in Player Stats - Batting
batting_stats_player <-
  read_rds("../../Data/T20s/Major League Cricket/t20_mlc_batting_innings_level.rds") |> 
  bind_rows(read_rds("../../Data/T20s/CPL/cpl_batting_innings_level.rds")) |> 
  bind_rows(read_rds("../../Data/T20s/Big Bash/bbl_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/IPL/ipl_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/The Hundred/the_hundred_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/Internationals/t20_international_batting_innings_level.rds") |> mutate(event = "T20I"))

# Read in Player Stats - Bowling
bowling_stats_player <-
  read_rds("../../Data/T20s/Major League Cricket/t20_mlc_bowling_innings_level.rds") |> 
  bind_rows(read_rds("../../Data/T20s/CPL/cpl_bowling_innings_level.rds")) |> 
  bind_rows(read_rds("../../Data/T20s/Big Bash/bbl_bowling_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/IPL/ipl_bowling_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/The Hundred/the_hundred_bowling_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/Internationals/t20_international_bowling_innings_level.rds") |> mutate(event = "T20I"))

# Tidy Names
batting_stats_player <-
  batting_stats_player |> 
  left_join(innings_stats[,c("match_id", "innings_1_balls", "innings_1_total", "innings_2_balls", "innings_2_total")], by = "match_id") |>
  mutate(innings_balls = ifelse(innings == 1, innings_1_balls, innings_2_balls)) |>
  mutate(team_runs = ifelse(innings == 1, innings_1_total, innings_2_total)) |>
  select(
    team1,
    team2,
    match_date,
    venue,
    event,
    toss_winner,
    toss_decision,
    winner,
    winner_runs,
    winner_wickets,
    player_full_name,
    player_unique_name,
    player_team,
    batting_position,
    batting_style,
    innings,
    team_runs,
    innings_balls,
    runs_scored,
    balls_faced,
    dismissal,
    fours,
    sixes,
    strike_rate) |> 
  mutate(dismissal = str_to_title(dismissal))

# Tidy Names
bowling_stats_player <-
  bowling_stats_player |> 
  left_join(innings_stats[,c("match_id", "innings_1_balls", "innings_1_total", "innings_2_balls", "innings_2_total")], by = "match_id") |>
  mutate(innings_balls = ifelse(innings == 1, innings_1_balls, innings_2_balls)) |>
  mutate(team_runs = ifelse(innings == 1, innings_1_total, innings_2_total)) |>
  select(
    team1,
    team2,
    match_date,
    venue,
    event,
    toss_winner,
    toss_decision,
    winner,
    winner_runs,
    winner_wickets,
    player_full_name,
    player_unique_name,
    player_team,
    bowling_style,
    innings,
    team_runs,
    innings_balls,
    balls_bowled,
    runs_conceded,
    fours_conceded,
    sixes_conceded,
    wickets,
    economy_rate
  )

# Unique Batters List
unique_players <-
  batting_stats_player |> 
  bind_rows(bowling_stats_player) |>
  distinct(player_full_name) |>
  pull(player_full_name)

# Unique Event List
unique_events <-
  batting_stats_player |> 
  bind_rows(bowling_stats_player) |>
  distinct(event) |>
  pull(event)

#===============================================================================
# Read in scraped odds
#===============================================================================

#===============================================================================
# UI
#===============================================================================

ui <- page_navbar(
  title = "Cricket Data",
  selected = "Player Stats",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  tags$head(tags$style(
    HTML(
      "
      .tab-content, .tab-pane {
        height: 1250px;
        overflow-y: auto;
      }
      .dataTables_wrapper {
        overflow-x: auto;
      }
    "
    )
  )),
  nav_panel(
    title = "Player Stats",
    grid_container(
      layout = c("stats player_stat_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "stats",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "player_name_input_a",
            label = "Select Player:",
            selected = "Sunil Philip Narine",
            choices = unique_players,
            selectize = TRUE
          ),
          selectInput(
            inputId = "event_input_a",
            label = "Select Event:",
            choices = unique_events,
            multiple = TRUE,
            selected = unique_events
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c("Runs",
                        "Wickets",
                        "4s",
                        "6s"),
            multiple = FALSE,
            selected = "Runs"
          ),
          selectInput(
            inputId = "venue_input_a",
            label = "Select Venue:",
            choices = batting_stats_player$venue |> unique() |> sort(),
            multiple = TRUE,
            selected = NULL,
            selectize = TRUE
          ),
          selectInput(
            inputId = "innings_input_a",
            label = "Select Innings:",
            choices = c("1", "2"),
            multiple = TRUE,
            selected = c("1", "2")
          ),
          markdown(mds = c("__Select Minimum Innings Length:__")),
          numericInput(
            inputId = "innings_balls_bowled_min",
            label = "Number of Balls",
            value = 0
          ),
          markdown(mds = c("__Select Only Last n Games:__")),
          numericInput(
            inputId = "last_games",
            label = "Number of Games",
            value = NA
          ),
          markdown(mds = c("__Select Reference Line:__")),
          numericInput(
            inputId = "reference_line",
            label = "Line Value",
            value = 19.5
          ))),
      grid_card(area = "player_stat_plot",
                card_body(
                  tabsetPanel(
                    id = "stat_tabs",
                    tabPanel("Plot",
                             plotOutput(outputId = "plot", height = "800px")),
                    tabPanel(
                      "Table",
                      DTOutput(
                        outputId = "player_stat_table",
                        width = "100%",
                        height = "800px"
                      )
                    )
                  )
                ))))
  ,
  nav_panel(
    title = "Team Stats",
    grid_container(
      layout = c("stats team_stat_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "stats",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "team_name_input_a",
            label = "Select Player:",
            selected = "Sunil Philip Narine",
            choices = unique_teams,
            selectize = TRUE
          ),
          selectInput(
            inputId = "event_input_a",
            label = "Select Event:",
            choices = unique_events,
            multiple = TRUE,
            selected = unique_events
          ),
          selectInput(
            inputId = "stat_input_a",
            label = "Select Statistic:",
            choices = c("Runs",
                        "Wickets",
                        "4s",
                        "6s"),
            multiple = FALSE,
            selected = "Runs"
          ),
          selectInput(
            inputId = "venue_input_a",
            label = "Select Venue:",
            choices = batting_stats_team$venue |> unique() |> sort(),
            multiple = TRUE,
            selected = NULL,
            selectize = TRUE
          ),
          selectInput(
            inputId = "innings_input_a",
            label = "Select Innings:",
            choices = c("1", "2"),
            multiple = TRUE,
            selected = c("1", "2")
          ),
          markdown(mds = c("__Select Minimum Innings Length:__")),
          numericInput(
            inputId = "innings_balls_bowled_min",
            label = "Number of Balls",
            value = 0
          ),
          markdown(mds = c("__Select Only Last n Games:__")),
          numericInput(
            inputId = "last_games",
            label = "Number of Games",
            value = NA
          ),
          markdown(mds = c("__Select Reference Line:__")),
          numericInput(
            inputId = "reference_line",
            label = "Line Value",
            value = 19.5
          ))),
      grid_card(area = "team_stat_plot",
                card_body(
                  tabsetPanel(
                    id = "stat_tabs",
                    tabPanel("Plot",
                             plotOutput(outputId = "plot", height = "800px")),
                    tabPanel(
                      "Table",
                      DTOutput(
                        outputId = "team_stat_table",
                        width = "100%",
                        height = "800px"
                      )
                    )
                  )
                ))))
  ,
  nav_panel(
    title = "Odds Screen",
    grid_container(
      layout = c("odds_screen odds_table"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "odds_screen",
        card_header("Settings"),
        card_body(
          # Add your specific input fields here
          # Example: selectInput, textInput, numericInput, etc.
          # selectInput(
          #   inputId = "agency_input",
          #   label = "Select Agencies:",
          #   choices = player_runs_data$agency |> unique(),
          #   multiple = TRUE,
          #   selectize = TRUE,
          #   selected = player_runs_data$agency |> unique(),
          # ),
          selectInput(
            inputId = "market_input",
            label = "Select Market:",
            choices = c("Runs", "Wickets", "Boundaries"),
            multiple = FALSE
          ),
          # selectInput(
          #   inputId = "match_input",
          #   label = "Select Matches:",
          #   choices = player_runs_data$match |> unique(),
          #   multiple = TRUE,
          #   selectize = FALSE,
          #   selected = player_runs_data$match |> unique()
          # ),
          textInput(
            inputId = "player_name_input_b",
            label = "Select Player:",
            value = NA
          ),
          checkboxInput(
            inputId = "only_unders",
            label = "Only Show Markets With Unders",
            value = FALSE
          ),
          checkboxInput(
            inputId = "only_best",
            label = "Only Show Best Market Odds - Overs",
            value = FALSE
          ),
          checkboxInput(
            inputId = "only_best_unders",
            label = "Only Show Best Market Odds - Unders",
            value = FALSE
          )
        )
      ),
      grid_card(area = "odds_table",
                card_body(
                  DTOutput(outputId = "scraped_odds_table", height = "1000px")
                ))
    )
  )
  # You can add more nav_panels here if needed for other sections of your new project
)

#===============================================================================
# Server
#===============================================================================

server <- function(input, output, session) {
  # Add your server-side code here
  
  #=============================================================================
  # Filter player stats
  #=============================================================================
  
  filtered_player_stats <- reactive({
    # Filter player stats
    
    if (input$stat_input_a == "Runs") {
      all_player_stats <- batting_stats_player
    } else if (input$stat_input_a == "Wickets") {
      all_player_stats <- bowling_stats_player
    } else if (input$stat_input_a == "4s") {
      all_player_stats <- batting_stats_player
    } else if (input$stat_input_a == "6s") {
      all_player_stats <- batting_stats_player
    }
    
    if (input$stat_input_a == "Wickets") {
      filtered_player_stats <-
        all_player_stats |>
        filter(
          player_full_name == input$player_name_input_a,
        ) |>
        arrange(match_date) |>
        mutate(game_number = row_number()) |> 
        select(Date = match_date,
               Event = event,
               Venue = venue,
               Innings = innings,
               Innings_Balls = innings_balls,
               Innings_Runs = team_runs,
               Player = player_full_name,
               Unique = player_unique_name,
               Team = player_team,
               Wickets = wickets,
               Runs = runs_conceded,
               Balls = balls_bowled,
               Fours = fours_conceded,
               Sixes = sixes_conceded,
               game_number) |> 
        arrange(desc(Date))
    }
    
    else {
      filtered_player_stats <-
        all_player_stats |>
        filter(
          player_full_name == input$player_name_input_a,
        ) |>
        arrange(match_date) |>
        mutate(game_number = row_number()) |> 
        select(Date = match_date,
               Event = event,
               Venue = venue,
               Innings = innings,
               Position = batting_position,
               Style = batting_style,
               Innings_Balls = innings_balls,
               Innings_Runs = team_runs,
               # DNB,
               Player = player_full_name,
               Unique = player_unique_name,
               Team = player_team,
               Runs = runs_scored,
               Balls = balls_faced,
               Dismissal = dismissal,
               `4s` = fours,
               `6s` = sixes,
               game_number) |> 
        arrange(desc(Date))
    }
    
    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_head(n = input$last_games)
    }
    
    # Filter by innings balls bowled
    if (!is.na(input$innings_balls_bowled_min)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Innings_Balls >= input$innings_balls_bowled_min)
    }
    
    # Filter by innings
    filtered_player_stats <-
      filtered_player_stats |>
      filter(Innings %in% input$innings_input_a)
    
    # Filter by event
    filtered_player_stats <-
      filtered_player_stats |>
      filter(Event %in% input$event_input_a)
    
    # Filter by venue
    if (!is.null(input$venue_input_a)) {
    filtered_player_stats <-
      filtered_player_stats |>
      filter(Venue %in% input$venue_input_a)
    }
    
    # Return filtered player stats
    return(filtered_player_stats)
    
  })
  
  #=============================================================================
  # Get Proportion above reference line
  #=============================================================================
  
  proportion_above_reference_line <- reactive({
    # Get proportion above reference line
    proportion_above_reference_line <-
      filtered_player_stats() |>
      filter(!!sym(input$stat_input_a) >= input$reference_line) |>
      nrow() / nrow(filtered_player_stats())
    
    # Get implied Odds
    implied_odds <- 1 / proportion_above_reference_line
    implied_odds_under <- 1 / (1 - proportion_above_reference_line)
    
    # Get string to output
    output_string <- paste0(
      "Proportion Above Reference Line: ",
      round(proportion_above_reference_line, 2),
      "\n",
      "Implied Odds - Over: ",
      round(implied_odds, 2),
      "\n",
      "Implied Odds - Under: ",
      round(implied_odds_under, 2),
      "\n",
      "Sample Size: ",
      nrow(filtered_player_stats())
    )
    
    return(output_string)
    
  })
  
  #=============================================================================
  # Plot player stats
  #=============================================================================
  
  output$plot <- renderPlot({
    # Create a new variable that checks if the y-value is above the reference line
    df_with_color <- filtered_player_stats() %>%
      mutate(color_condition = ifelse(
        !!sym(input$stat_input_a) >= input$reference_line,
        "limegreen",
        "red1"
      ))
    
    # Plot player stats
    p <- df_with_color %>%
      ggplot(aes(
        x = game_number,
        y = !!sym(input$stat_input_a),
        color = color_condition
      )) +
      
      # Basic Elements
      geom_point(size = 3) +
      geom_smooth(
        method = "loess",
        se = FALSE,
        inherit.aes = FALSE,
        mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
      ) +
      geom_hline(
        yintercept = input$reference_line,
        linetype = "dashed",
        color = "grey4",
        size = 1
      )+
      
      # Add text
      annotate(
        geom = "text",
        x = 1,
        y = max(filtered_player_stats() %>% pull(!!sym(
          input$stat_input_a
        ))),
        label = proportion_above_reference_line(),
        hjust = 0,
        vjust = 1,
        color = "black",
        size = 6
      ) +
      
      # Aesthetics
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      
      # Labels & Titles
      labs(title = "",
           x = "Game Number") +
      
      # Set manual color scale
      scale_color_identity() +
      
      # Additional
      theme(legend.position = "none")
    
    print(p)
  })
  
  #=============================================================================
  # Table player stats
  #=============================================================================
  
  output$player_stat_table <- renderDT({
    datatable(
      filtered_player_stats(),
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
      width = "100%",
      height = "800px"
    )
  })
  
  #=============================================================================
  # Reactive function to scrape odds
  #=============================================================================
  
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------
    
    # Runs
    if (input$market_input == "Runs") {
      odds <-
        player_runs_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    # Boundaries
    if (input$market_input == "Boundaries") {
      odds <-
        player_boundaries_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match) 
    }
    
    # Wickets
    if (input$market_input == "Wickets") {
      odds <-
        player_wickets_data |> 
        mutate(variation = round(variation, 2)) |>
        filter(match %in% input$match_input) |>
        select(-match)
    }
    
    if (input$player_name_input_b != "") {
      odds <-
        odds |>
        filter(str_detect(player_name, input$player_name_input_b))
    }
    
    if (input$only_best == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(over_price)) |>
        group_by(player_name, market, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    if (input$only_best_unders == TRUE) {
      odds <-
        odds |> 
        arrange(player_name, line, desc(under_price)) |>
        group_by(player_name, market, line) |> 
        slice_head(n = 1) |>
        ungroup()
    }
    
    if (input$only_unders == TRUE) {
      odds <-
        odds |> 
        filter(!is.na(under_price))
    }
    
    # Return odds
    return(odds)
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
              fillContainer = TRUE,
              filter = "top",
              options = list(
                pageLength = 17,
                autoWidth = FALSE,
                scrollX = TRUE, scrollY = TRUE,
                lengthMenu = c(5, 10, 15, 20, 25, 30)
              ))
  })
}

# Run the application
shinyApp(ui, server)