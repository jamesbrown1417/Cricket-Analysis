#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd "/Users/jamesbrown/Projects/Cricket-Analysis" || exit

# Remove .txt files in specific directories
rm -f "Odds-Scraping/BBL/Bet365/HTML/*.txt"
rm -f Data/T20s/Big\ Bash/scraped_odds/*

# Execute Python scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/Bet365/bet365_bbl_scraper.py"
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 "Odds-Scraping/BBL/TAB/get-TAB-response.py"

# Execute R scripts
Rscript "Odds-Scraping/BBL/Bet365/bet365_bbl_markets.R"

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
