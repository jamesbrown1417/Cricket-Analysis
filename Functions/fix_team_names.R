# Function to fix team names
fix_team_names <- function(team_name_vector) {
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