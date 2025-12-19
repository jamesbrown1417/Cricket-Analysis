# Function to fix team names
fix_team_names_bbl <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Renegades") ~ "Melbourne Renegades",
    str_detect(team_name_vector, "Stars") ~ "Melbourne Stars",
    str_detect(team_name_vector, "Sixers") ~ "Sydney Sixers",
    str_detect(team_name_vector, "Thunder") ~ "Sydney Thunder",
    str_detect(team_name_vector, "Strikers") ~ "Adelaide Strikers",
    str_detect(team_name_vector, "Heat") ~ "Brisbane Heat",
    str_detect(team_name_vector, "Scorchers") ~ "Perth Scorchers",
    str_detect(team_name_vector, "Hurricanes") ~ "Hobart Hurricanes",
    TRUE ~ team_name_vector
  )
}

# Function to fix team names for Major League Cricket (MLC)
fix_team_names_mlc <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "MI|Mumbai|New York") ~ "MI New York",
    str_detect(team_name_vector, "Orcas|Seattle") ~ "Seattle Orcas",
    str_detect(team_name_vector, "Unicorns|San Francisco|SF") ~ "San Francisco Unicorns",
    str_detect(team_name_vector, "Super Kings|Texas|TSK") ~ "Texas Super Kings",
    str_detect(team_name_vector, "Knight Riders|Los Angeles|LA") ~ "Los Angeles Knight Riders",
    str_detect(team_name_vector, "Freedom|Washington|DC") ~ "Washington Freedom",
    TRUE ~ team_name_vector
  )
}

# Function to fix team names for Caribbean Premier League (CPL)
fix_team_names_cpl <- function(team_name_vector) {
  team_name_vector <- case_when(
    str_detect(team_name_vector, "Antigua And Barb|Antigua") ~ "Antigua and Barbuda Falcons",
    str_detect(team_name_vector, "St. Kitts|St Kitts|StK") ~ "St Kitts and Nevis Patriots",
    str_detect(team_name_vector, "Guyana|Guy") ~ "Guyana Amazon Warriors",
    str_detect(team_name_vector, "Jamaica") ~ "Jamaica Tallawahs",
    str_detect(team_name_vector, "Barbados|Brb|Bar") ~ "Barbados Royals",
    str_detect(team_name_vector, "Trinbago|Tbg") ~ "Trinbago Knight Riders",
    str_detect(team_name_vector, "St. Lucia|St Lucia|Saint Lucia|StL") ~ "St Lucia Kings",
    TRUE ~ team_name_vector
  )
}