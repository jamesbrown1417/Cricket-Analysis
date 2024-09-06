# Set the current directory to your project folder
Set-Location -Path "C:\Users\a1645197\Cricket-Analysis"

# Remove .json and .txt files in specific directories
# Remove-Item -Path "C:\Users\a1645197\Cricket-Analysis\Odds-Scraping\Internationals\Neds\*.json"
Remove-Item -Path "C:\Users\a1645197\Cricket-Analysis\Odds-Scraping\Internationals\Bet365\HTML\*.txt"

# Execute Python and R scripts
& "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Bet365/01-get_bet365_html.py"
& "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Bet365/02-get_bet365_player.py"
& "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Bet365/03-get_bet365_match.py"
& "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Bet365/04-get_bet365_team.py"
& "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Bet365/05-get_bet365_first_over.py"
& "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Bet365/06-get_bet365_first_innings.py"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-fall-of-first-wicket.R"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-first-over-runs.R"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-h2h.R"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-player-runs.R"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-bowerl-wickets.R"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-team-sixes-and-fours.R"
& "Rscript" "Odds-Scraping\Internationals\Bet365\-bet365-match-sixes-and-fours.R"

# & "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds-Scraping/Internationals/Neds/get_neds_urls_Internationals.py"
# & "Rscript" "Odds-Scraping\Internationals\Neds\get_neds_match_urls_Internationals.R"
# & "C:/Users/a1645197/.pyenv/pyenv-win/versions/3.12.5/python.exe" "c:/Users/a1645197/Cricket-Analysis/Odds/Scraping/Internationals/Neds/get_match_json_Internationals.py"

# Execute R script for getting processed odds
& "Rscript" "Odds-Scraping\Internationals\master_processing_script.R"

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
$commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
git commit -m $commitMessage

# Push the commit to the 'master' branch on 'origin'
git push origin master