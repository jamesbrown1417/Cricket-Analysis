library(shiny)
library(bslib)
library(gridlayout)
library(DT)
library(tidyverse)

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
  bind_rows(read_rds("../../Data/T20s/LPL/lpl_match_innings_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/Internationals/t20_international_match_innings_data.rds")|> mutate(event = "T20I"))

# Get Innings in long format
innings_stats_1 <-
  innings_stats |> 
  mutate(match_winner = if_else(innings_1_total > innings_2_total, innings_1_batting_team, innings_2_batting_team)) |> 
  mutate(chasing_team_won = as.numeric(match_winner == innings_2_batting_team)) |> 
  select(match_id, match_date, event, toss_winner, toss_decision, player_of_the_match, venue, match_winner, chasing_team_won,
         innings_1_batting_team:innings_1_method_of_first_dismissal, innings_fielding_team = innings_2_batting_team) |> 
  mutate(innings = 1) |> 
  # Remove _1 from column names
  rename_with(~str_remove(., "_1"), starts_with("innings_1")) |> 
  relocate(innings, .after = venue)

innings_stats_2 <-
  innings_stats |> 
  mutate(match_winner = if_else(innings_1_total > innings_2_total, innings_1_batting_team, innings_2_batting_team)) |> 
  mutate(chasing_team_won = as.numeric(match_winner == innings_2_batting_team)) |> 
  select(match_id, match_date, event, toss_winner, toss_decision, player_of_the_match, venue,  match_winner, chasing_team_won,
         innings_2_batting_team:innings_2_method_of_first_dismissal, innings_fielding_team = innings_1_batting_team) |> 
  mutate(innings = 2) |> 
  # Remove _2 from column names
  rename_with(~str_remove(., "_2"), starts_with("innings_2")) |> 
  relocate(innings, .after = venue)

innings_stats_long <-
  bind_rows(innings_stats_1, innings_stats_2) |> 
  arrange(desc(match_date), match_id, innings) |> 
  mutate(first_wicket_caught = if_else(innings_method_of_first_dismissal %in% c("caught", "caught and bowled"), 1, 0))

# Read in First Over Stats
first_over_stats <-
  read_rds("../../Data/T20s/Major League Cricket/t20_mlc_first_over_data.rds") |> 
  bind_rows(read_rds("../../Data/T20s/CPL/cpl_first_over_data.rds")) |> 
  bind_rows(read_rds("../../Data/T20s/Big Bash/bbl_first_over_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/IPL/ipl_first_over_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/The Hundred/the_hundred_first_over_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/LPL/lpl_first_over_data.rds")) |>
  bind_rows(read_rds("../../Data/T20s/Internationals/t20_international_match_first_over_data.rds")|> mutate(event = "T20I"))

# Join With Innings Stats
innings_stats_long <-
  innings_stats_long |> 
  left_join(first_over_stats)

# Read in Player Stats - Batting
batting_stats_player <-
  read_rds("../../Data/T20s/Major League Cricket/t20_mlc_batting_innings_level.rds") |> 
  bind_rows(read_rds("../../Data/T20s/CPL/cpl_batting_innings_level.rds")) |> 
  bind_rows(read_rds("../../Data/T20s/Big Bash/bbl_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/IPL/ipl_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/The Hundred/the_hundred_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/LPL/lpl_batting_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/Internationals/t20_international_batting_innings_level.rds") |> mutate(event = "T20I"))

# Read in Player Stats - Bowling
bowling_stats_player <-
  read_rds("../../Data/T20s/Major League Cricket/t20_mlc_bowling_innings_level.rds") |> 
  bind_rows(read_rds("../../Data/T20s/CPL/cpl_bowling_innings_level.rds")) |> 
  bind_rows(read_rds("../../Data/T20s/Big Bash/bbl_bowling_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/IPL/ipl_bowling_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/The Hundred/the_hundred_bowling_innings_level.rds")) |>
  bind_rows(read_rds("../../Data/T20s/LPL/lpl_bowling_innings_level.rds")) |>
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

# Unique Players List
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

# Unique Team List
unique_teams <-
  innings_stats |>
  distinct(innings_1_batting_team) |>
  pull(innings_1_batting_team)

#===============================================================================
# Read in processed odds
#===============================================================================

# BBL---------------------------------------------------------------------------
all_files_bbl <- list.files("../../Data/T20s/Big Bash/processed_odds/", full.names = TRUE, pattern = "rds")

# Get names
all_files_bbl_names <- all_files_bbl |> str_remove("../../Data/T20s/Big Bash/processed_odds/") |> str_remove(".rds")

# Read in files in a loop
bbl_odds <-
  map(all_files_bbl, read_rds) |> 
  set_names(all_files_bbl_names)

# Combine all
player_runs <- bind_rows(bbl_odds$player_runs)
player_wickets <- bind_rows(bbl_odds$player_wickets)
player_boundaries <- bind_rows(bbl_odds$player_boundaries)
fall_of_first_wicket <- bind_rows(bbl_odds$runs_at_first_wicket)
first_over_runs <- bind_rows(bbl_odds$first_over_runs)
match_sixes <- bind_rows(bbl_odds$match_sixes)
team_sixes <- bind_rows(bbl_odds$team_sixes)
match_fours <- bind_rows(bbl_odds$match_fours)
team_fours <- bind_rows(bbl_odds$team_fours)

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
            selected = "Matthew William Short",
            choices = unique_players,
          ),
          selectInput(
            inputId = "event_input_a",
            label = "Select Event:",
            choices = unique_events,
            multiple = TRUE,
            selected = "Big Bash League"
          ),
          dateInput(
            inputId = "start_date",
            label = "Select Start Date:",
            value = "2023-01-12"),
          dateInput(
            inputId = "end_date",
            label = "Select End Date:"),
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
                             plotOutput(outputId = "player_stat_plot", height = "800px")),
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
      layout = c("team_stats team_stat_plot"),
      row_sizes = c("1fr"),
      col_sizes = c("250px", "1fr"),
      gap_size = "10px",
      grid_card(
        area = "team_stats",
        card_header("Settings"),
        card_body(
          selectInput(
            inputId = "team_name_input_c",
            label = "Select Team:",
            choices = unique_teams,
            multiple = TRUE,
            selectize = TRUE
          ),
          selectInput(
            inputId = "event_input_c",
            label = "Select Event:",
            choices = unique_events,
            multiple = TRUE,
            selected = "Big Bash League"
          ),
          dateInput(
            inputId = "start_date_team",
            label = "Select Start Date:",
            value = "2023-01-12"),
          dateInput(
            inputId = "end_date_team",
            label = "Select End Date:"),
          selectInput(
            inputId = "stat_input_c",
            label = "Select Statistic:",
            choices = c("Runs",
                        "Wickets",
                        "Runs at Fall of First Wicket",
                        "First Wicket Caught",
                        "First Over Runs",
                        "Chasing Team Win",
                        "4s",
                        "6s",
                        "Total Match Runs",
                        "Total Match Wickets",
                        "Total Match 4s",
                        "Total Match 6s"
                        ),
            multiple = FALSE,
            selected = "Innings Total"
          ),
          selectInput(
            inputId = "venue_input_c",
            label = "Select Venue:",
            choices = batting_stats_player$venue |> unique() |> sort(),
            multiple = TRUE,
            selected = NULL,
            selectize = TRUE
          ),
          selectInput(
            inputId = "innings_input_c",
            label = "Select Innings:",
            choices = c("1", "2"),
            multiple = TRUE,
            selected = c("1", "2")
          ),
          markdown(mds = c("__Select Minimum Innings Length:__")),
          numericInput(
            inputId = "innings_balls_bowled_min_c",
            label = "Number of Balls",
            value = 0
          ),
          markdown(mds = c("__Select Only Last n Games:__")),
          numericInput(
            inputId = "last_games_c",
            label = "Number of Games",
            value = NA
          ),
          markdown(mds = c("__Select Reference Line:__")),
          numericInput(
            inputId = "reference_line_c",
            label = "Line Value",
            value = 149.5
          ))),
      grid_card(area = "team_stat_plot",
                card_body(
                  tabsetPanel(
                    id = "stat_tabs_team",
                    tabPanel("Plot",
                             plotOutput(outputId = "team_stat_plot", height = "800px")),
                    tabPanel("Summary",
                             DTOutput(outputId = "team_stat_summary",width = "100%", height = "800px")),
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
          selectInput(
            inputId = "market_input",
            label = "Select Market:",
            choices = c("Player Runs", "Fall of First Wicket - Team", "First Over Runs - Team", "Match Sixes", "Team Sixes", "Match Fours", "Team Fours", "Player Wickets", "Player Boundaries"),
            multiple = FALSE
          ),
          selectInput(
            inputId = "match_input_odds",
            label = "Select Match:",
            choices = player_runs$match,
            selected = player_runs$match,
            multiple = TRUE
          ),
          selectInput(
            inputId = "competition_input",
            label = "Select Competition:",
            choices = c("BBL"),
            selected = c("BBL"),
            multiple = TRUE
          ),
          selectInput(
            inputId = "player_name_input_b",
            label = "Select Player:",
            choices = unique(c(player_runs$player_name, player_wickets$player_name, player_boundaries$player_name)),
            multiple = TRUE
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
    
    # Filter by date
    if (!is.null(input$start_date)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Date >= input$start_date)
    }
    
    if (!is.null(input$end_date)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Date <= input$end_date)
    }
    
    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_head(n = input$last_games)
    }
    
    filtered_player_stats <-
    filtered_player_stats |> 
      mutate(game_number = row_number())
    
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
  
  output$player_stat_plot <- renderPlot({
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
  # Filter Team stats
  #=============================================================================
  
  filtered_team_stats <- reactive({
    # Filter team stats
      filtered_team_stats <-
        innings_stats_long |>
        select(Date = match_date,
               Event = event,
               Venue = venue,
               `Toss Winner` = toss_winner,
               `Toss Decision` = toss_decision,
               `Match Winner` = match_winner,
               `Chasing Team Win` = chasing_team_won,
               Innings = innings,
               Innings_Balls = innings_balls,
               Runs = innings_total,
               `4s` = innings_fours,
               `6s` = innings_sixes,
               Wickets = innings_wickets,
               Batting = innings_batting_team,
               Fielding = innings_fielding_team, 
               `First Dismissal Method` = innings_method_of_first_dismissal,
               `First Wicket Caught` = first_wicket_caught,
               `Runs at Fall of First Wicket` = innings_fall_of_first_wicket,
               `First Over Bowler` = first_over_bowler,
               `First Over Runs` = first_over_total,
               `First Over Wickets` = first_over_wickets,
               `First Over 4s` = first_over_fours,
               `First Over 6s` = first_over_sixes) |> 
        arrange(desc(Date))
    
    # Filter by innings balls bowled
    if (!is.na(input$innings_balls_bowled_min_c)) {
      filtered_team_stats <-
        filtered_team_stats |>
        filter(Innings_Balls >= input$innings_balls_bowled_min_c)
    }
    
    # Filter by innings
    filtered_team_stats <-
      filtered_team_stats |>
      filter(Innings %in% input$innings_input_c)
    
    # Filter by event
    filtered_team_stats <-
      filtered_team_stats |>
      filter(Event %in% input$event_input_c)
    
    # Filter by venue  
    if (!is.null(input$venue_input_c)) {
      filtered_team_stats <-
        filtered_team_stats |>
        filter(Venue %in% input$venue_input_c)
    }
    
    # Filter by team
    if (!is.null(input$team_name_input_c)) {
      filtered_team_stats <-
        filtered_team_stats |>
        filter(Batting %in% input$team_name_input_c)
    }
    
    # Filter by last n games
    if (!is.na(input$last_games_c)) {
      filtered_team_stats <-
        filtered_team_stats |>
        slice_head(n = input$last_games_c)
    }
    
    # Filter by start date
    filtered_team_stats <-
      filtered_team_stats |>
      filter(Date >= input$start_date_team) |> 
      filter(Date <= input$end_date_team)
    
    # Now make game number
    filtered_team_stats <-
      filtered_team_stats |>
      mutate(game_number = row_number())
    
    # Return filtered player stats
    return(filtered_team_stats)
    
  })
  
  #=============================================================================
  # Get match instead of innings stats
  #=============================================================================
  
  filtered_match_stats <- reactive({
    # Get match stats
    filtered_match_stats <-
      innings_stats_long |>
      group_by(match_id) |>
      summarise(
        Date = first(match_date),
        Event = first(event),
        Venue = first(venue),
        Team_1 = first(first_over_batting_team),
        Team_2 = first(first_over_bowling_team),
        `Match Winner` = first(match_winner),
        `Chasing Team Win` = first(chasing_team_won),
        `Toss Winner` = first(toss_winner),
        `Toss Decision` = first(toss_decision),
        `Total Match Runs` = sum(innings_total),
        `Total Match Wickets` = sum(innings_wickets),
        `Total Match 4s` = sum(innings_fours),
        `Total Match 6s` = sum(innings_sixes)
      )
    
    # Filter by event
    filtered_match_stats <-
      filtered_match_stats |>
      filter(Event %in% input$event_input_c)
    
    # Filter by venue  
    if (!is.null(input$venue_input_c)) {
      filtered_match_stats <-
        filtered_match_stats |>
        filter(Venue %in% input$venue_input_c)
    }
    
    # Filter by team
    if (!is.null(input$team_name_input_c)) {
      filtered_match_stats <-
        filtered_match_stats |>
        filter(Team_1 %in% input$team_name_input_c | Team_2 %in% input$team_name_input_c)
    }
    
    # Filter by last n games
    if (!is.na(input$last_games_c)) {
      filtered_match_stats <-
        filtered_match_stats |>
        slice_head(n = input$last_games_c)
    }
    
    # Filter by start date
      filtered_match_stats <-
        filtered_match_stats |>
        filter(Date >= input$start_date_team) |> 
        filter(Date <= input$end_date_team)
    
    # Now make game number
    filtered_match_stats <-
      filtered_match_stats |>
      mutate(game_number = row_number())
    
    # Return match stats
    return(filtered_match_stats)
  })
  
  #=============================================================================
  # Get Proportion above reference line
  #=============================================================================
  
  proportion_above_reference_line_team <- reactive({
    
    if (input$stat_input_c %in% c("Total Match Runs", "Total Match Wickets", "Total Match 4s", "Total Match 6s")) {
      # Get proportion above reference line
      proportion_above_reference_line_team <-
        filtered_match_stats() |>
        filter(!!sym(input$stat_input_c) >= input$reference_line_c) |>
        nrow() / nrow(filtered_match_stats())
      
      # Get implied Odds
      implied_odds <- 1 / proportion_above_reference_line_team
      implied_odds_under <- 1 / (1 - proportion_above_reference_line_team)
      
      # Get string to output
      output_string_team <- paste0(
        "Proportion Above Reference Line: ",
        round(proportion_above_reference_line_team, 2),
        "\n",
        "Implied Odds - Over: ",
        round(implied_odds, 2),
        "\n",
        "Implied Odds - Under: ",
        round(implied_odds_under, 2),
        "\n",
        "Sample Size: ",
        nrow(filtered_match_stats())
      )
      
      return(output_string_team)
    }
    
    else {
      # Get proportion above reference line
      proportion_above_reference_line_team <-
        filtered_team_stats() |>
        filter(!!sym(input$stat_input_c) >= input$reference_line_c) |>
        nrow() / nrow(filtered_team_stats())
      
      # Get implied Odds
      implied_odds <- 1 / proportion_above_reference_line_team
      implied_odds_under <- 1 / (1 - proportion_above_reference_line_team)
      
      # Get string to output
      output_string_team <- paste0(
        "Proportion Above Reference Line: ",
        round(proportion_above_reference_line_team, 2),
        "\n",
        "Implied Odds - Over: ",
        round(implied_odds, 2),
        "\n",
        "Implied Odds - Under: ",
        round(implied_odds_under, 2),
        "\n",
        "Sample Size: ",
        nrow(filtered_team_stats())
      )
      
      return(output_string_team)
    }
    
    
    
  })
  
  #=============================================================================
  # Plot team stats
  #=============================================================================
  
  output$team_stat_plot <- renderPlot({
    if (input$stat_input_c %in% c("Total Match Runs", "Total Match Wickets", "Total Match 4s", "Total Match 6s")) {
      # Create a new variable that checks if the y-value is above the reference line
      df_with_color <- filtered_match_stats() %>%
        mutate(color_condition = ifelse(
          !!sym(input$stat_input_c) >= input$reference_line_c,
          "limegreen",
          "red1"
        ))
      
      # Plot player stats
      p <- df_with_color %>%
        ggplot(aes(
          x = game_number,
          y = !!sym(input$stat_input_c),
          color = color_condition
        )) +
        
        # Basic Elements
        geom_point(size = 3) +
        geom_smooth(
          method = "loess",
          se = FALSE,
          inherit.aes = FALSE,
          mapping = aes(x = game_number, y = !!sym(input$stat_input_c))
        ) +
        geom_hline(
          yintercept = input$reference_line_c,
          linetype = "dashed",
          color = "grey4",
          size = 1
        )+
        
        # Add text
        annotate(
          geom = "text",
          x = 1,
          y = max(filtered_match_stats() %>% pull(!!sym(
            input$stat_input_c
          ))),
          label = proportion_above_reference_line_team(),
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
    }
    
    else {
      # Create a new variable that checks if the y-value is above the reference line
      df_with_color <- filtered_team_stats() %>%
        mutate(color_condition = ifelse(
          !!sym(input$stat_input_c) >= input$reference_line_c,
          "limegreen",
          "red1"
        ))
      
      # Plot player stats
      p <- df_with_color %>%
        ggplot(aes(
          x = game_number,
          y = !!sym(input$stat_input_c),
          color = color_condition
        )) +
        
        # Basic Elements
        geom_point(size = 3) +
        geom_smooth(
          method = "loess",
          se = FALSE,
          inherit.aes = FALSE,
          mapping = aes(x = game_number, y = !!sym(input$stat_input_c))
        ) +
        geom_hline(
          yintercept = input$reference_line_c,
          linetype = "dashed",
          color = "grey4",
          size = 1
        )+
        
        # Add text
        annotate(
          geom = "text",
          x = 1,
          y = max(filtered_team_stats() %>% pull(!!sym(
            input$stat_input_c
          ))),
          label = proportion_above_reference_line_team(),
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
    }
   
  })
  
  #=============================================================================
  # Table team stats
  #=============================================================================
  
  output$team_stat_table <- renderDT({
    if (input$stat_input_c %in% c("Total Match Runs", "Total Match Wickets", "Total Match 4s", "Total Match 6s")) {
      datatable(
        filtered_match_stats(),
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
        width = "100%",
        height = "800px"
      )
    }
    else {
      datatable(
        filtered_team_stats(),
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
        width = "100%",
        height = "800px"
      )
    }
  })
  
  #=============================================================================
  # Table team summary
  #=============================================================================
  
  output$team_stat_summary <- renderDT({
    if (input$stat_input_c %in% c("Total Match Runs", "Total Match Wickets", "Total Match 4s", "Total Match 6s")) {
      datatable(
        filtered_match_stats() |> 
          summarise(
            mean = round(mean(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            median = round(median(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            sd = round(sd(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            min = round(min(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            max = round(max(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            n = n()  
          ),
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
        width = "100%",
        height = "800px"
      )
      }
    else {
      datatable(
        filtered_team_stats() |> 
          summarise(
            mean = round(mean(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            median = round(median(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            sd = round(sd(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            min = round(min(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            max = round(max(!!sym(input$stat_input_c), na.rm = TRUE), 2),
            n = n()  
          ),
        options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE),
        width = "100%",
        height = "800px"
      ) 
    }
  })
  
  #=============================================================================
  # Reactive function to scrape odds
  #=============================================================================
  
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------
    
    # Player Runs
    if (input$market_input == "Player Runs") {
      odds <-
        player_runs
    }
    
    # Player Boundaries
    if (input$market_input == "Player Boundaries") {
      odds <-
        player_boundaries
    }
    
    # Player Wickets
    if (input$market_input == "Player Wickets") {
      odds <-
        player_wickets
    }
    
    # Fall of First Wicket
    if (input$market_input == "Fall of First Wicket - Team") {
      odds <-
        fall_of_first_wicket
    }
    
    # First Over Runs
    if (input$market_input == "First Over Runs - Team") {
      odds <-
        first_over_runs
    }
    
    # Match Sixes
    if (input$market_input == "Match Sixes") {
      odds <-
        match_sixes
    }
    
    # Team Sixes
    if (input$market_input == "Team Sixes") {
      odds <-
        team_sixes
    }
    
    # Match Fours
    if (input$market_input == "Match Fours") {
      odds <-
        match_fours
    }
    
    # Team Fours
    if (input$market_input == "Team Fours") {
      odds <-
        team_fours
    }
    
    if (input$market_input %in% c("Player Runs", "Player Wickets", "Player Boundaries")) {
      if (!is.null(input$player_name_input_b)) {
        odds <-
          odds |>
          filter(player_name %in% input$player_name_input_b)
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
    }
    
    odds <-
      odds |>
      filter(competition %in% input$competition_input) |> 
      filter(match %in% input$match_input_odds)
    
    # Return odds
    return(odds)
  })
  
  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
              fillContainer = TRUE,
              filter = "top",
              options = list(
                pageLength = 15,
                autoWidth = FALSE,
                scrollX = TRUE, scrollY = TRUE,
                lengthMenu = c(5, 10, 15, 20, 25, 30)
              ))
  })
}

# Run the application
shinyApp(ui, server)