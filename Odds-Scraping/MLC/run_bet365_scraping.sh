#!/bin/bash

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to project folder
cd /Users/jamesbrown/Projects/Cricket-Analysis || exit

# Remove all files from the HTML folder to start fresh
echo "Cleaning HTML folder..."
rm -f Odds-Scraping/MLC/Bet365/HTML/*

# Remove existing CSV files from the scraped odds directory
echo "Cleaning existing bet365 CSV files..."
rm -f "Data/T20s/Major League Cricket/scraped_odds/"*.csv

# Execute Python scripts for HTML generation (in order)
echo "Running Python scripts to generate HTML files..."

echo "1. Getting bet365 HTML and URLs..."
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Odds-Scraping/MLC/Bet365/01-get_bet365_html.py

echo "2. Getting player HTML..."
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Odds-Scraping/MLC/Bet365/02-get_bet365_player.py

echo "3. Getting match HTML..."
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Odds-Scraping/MLC/Bet365/03-get_bet365_match.py

echo "4. Getting team HTML..."
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Odds-Scraping/MLC/Bet365/04-get_bet365_team.py

echo "5. Getting first over HTML..."
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Odds-Scraping/MLC/Bet365/05-get_bet365_first_over.py

echo "6. Getting first innings HTML..."
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Odds-Scraping/MLC/Bet365/06-get_bet365_first_innings.py

# Execute R scripts for data scraping (in order)
echo "Running R scripts to scrape odds data..."

echo "1. Scraping H2H odds..."
Rscript Odds-Scraping/MLC/Bet365/01-bet365-h2h.R

echo "2. Scraping player runs..."
Rscript Odds-Scraping/MLC/Bet365/02-bet365-player-runs.R

echo "3. Scraping player boundaries..."
Rscript Odds-Scraping/MLC/Bet365/03-bet365-player-boundaries.R

echo "4. Scraping bowler wickets..."
Rscript Odds-Scraping/MLC/Bet365/04-bet365-bowler-wickets.R

echo "5. Scraping match most sixes and fours..."
Rscript Odds-Scraping/MLC/Bet365/05-bet365-match-most-sixes-and-fours.R

echo "6. Scraping match sixes and fours..."
Rscript Odds-Scraping/MLC/Bet365/06-bet365-match-sixes-and-fours.R

echo "7. Scraping team sixes and fours..."
Rscript Odds-Scraping/MLC/Bet365/07-bet365-team-sixes-and-fours.R

echo "8. Scraping fall of first wicket..."
Rscript Odds-Scraping/MLC/Bet365/08-bet365-fall-of-first-wicket.R

echo "9. Scraping first over runs..."
Rscript Odds-Scraping/MLC/Bet365/09-bet365-first-over-runs.R

echo "Bet365 scraping completed successfully!"