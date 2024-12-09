# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\OneDrive\Desktop\Projects\Cricket-Analysis"

# Remove .json and .txt files in specific directories
# Remove-Item -Path "C:\Users\james\OneDrive\Desktop\Projects\Cricket-Analysis\Odds-Scraping\CPL\Neds\*.json"
Remove-Item -Path "C:\Users\james\OneDrive\Desktop\Projects\Cricket-Analysis\Odds-Scraping\CPL\Bet365\HTML\*.txt"

# Execute Python and R scripts
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Bet365/01-get_bet365_html.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Bet365/02-get_bet365_player.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Bet365/03-get_bet365_match.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Bet365/04-get_bet365_team.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Bet365/05-get_bet365_first_over.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Bet365/06-get_bet365_first_innings.py"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/TAB/get-TAB-response.py"

& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-fall-of-first-wicket.R"
& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-first-over-runs.R"
& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-h2h.R"
& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-player-runs.R"
& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-bowerl-wickets.R"
& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-team-sixes-and-fours.R"
& "Rscript" "Odds-Scraping\CPL\Bet365\-bet365-match-sixes-and-fours.R"

# & "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Neds/get_neds_urls_CPL.py"
# & "Rscript" "Odds-Scraping\CPL\Neds\get_neds_match_urls_CPL.R"
# & "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "c:/Users/james/OneDrive/Desktop/Projects/Cricket-Analysis/Odds-Scraping/CPL/Neds/get_match_json_CPL.py"

# Execute R script for getting processed odds
& "Rscript" "Odds-Scraping\CPL\master_processing_script.R"

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
$commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
git commit -m $commitMessage

# Push the commit to the 'master' branch on 'origin'
git push origin master

# Publish report using Quarto
echo "1" | & "quarto" "publish" "quarto-pub" "Odds-Scraping\CPL\CPL_Report.qmd"