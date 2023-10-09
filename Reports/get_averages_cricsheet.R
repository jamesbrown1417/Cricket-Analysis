match_data |> 
  group_by(striker) |> 
  summarise(runs = sum(runs_off_bat), balls_faced = n(), wickets = sum(striker == player_dismissed)) |> 
  mutate(average = runs / wickets) |> 
  mutate(strike_rate = 100*(runs / balls_faced)) |> 
  arrange(desc(runs)) |>
  filter(runs > 500)

match_data |> 
  group_by(bowler) |> 
  summarise(runs = sum(runs_off_bat), balls_bowled = n(), wickets = sum(wicket_type %in% c("bowled", "caught", "caught and bowled", "hit wicket", "lbw", "stumped"))) |> 
  mutate(average = runs / wickets) |> 
  mutate(strike_rate = (balls_bowled / wickets)) |> 
  arrange(desc(wickets)) |>
  filter(wickets > 25)
