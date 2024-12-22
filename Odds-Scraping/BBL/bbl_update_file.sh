#!/bin/bash

# Set the current directory to your project folder
cd "/Users/jamesbrown/Projects/Cricket-Analysis" || exit

# Remove .txt files in specific directories
rm -f "Odds-Scraping/BBL/Bet365/HTML/*.txt"

# Execute Python scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/01-get_bet365_html.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/02-get_bet365_player.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/03-get_bet365_match.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/04-get_bet365_team.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/05-get_bet365_first_over.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/06-get_bet365_first_innings.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/TAB/get-TAB-response.py"

# Execute R scripts
Rscript "Odds-Scraping/BBL/Bet365/-bet365-fall-of-first-wicket.R"
Rscript "Odds-Scraping/BBL/Bet365/-bet365-first-over-runs.R"
Rscript "Odds-Scraping/BBL/Bet365/-bet365-h2h.R"
Rscript "Odds-Scraping/BBL/Bet365/-bet365-player-runs.R"
Rscript "Odds-Scraping/BBL/Bet365/-bet365-bowerl-wickets.R"
Rscript "Odds-Scraping/BBL/Bet365/-bet365-team-sixes-and-fours.R"
Rscript "Odds-Scraping/BBL/Bet365/-bet365-match-sixes-and-fours.R"

# Execute R script for processed odds
Rscript "Odds-Scraping/BBL/master_processing_script.R"

# Automatically stage all changes
git add .

# Commit changes with a message including the current timestamp
commit_message="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commit_message"

# Push the commit to the 'master' branch on 'origin'
git push origin master

# Create Report
echo "1" | quarto publish quarto-pub "Odds-Scraping/BBL/BBL_Report.qmd"