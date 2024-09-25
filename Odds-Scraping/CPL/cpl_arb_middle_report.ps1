# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\OneDrive\Desktop\Projects\Cricket-Analysis"

# Publish report using Quarto
echo "1" | & "quarto" "publish" "quarto-pub" "Odds-Scraping\CPL\CPL_Report.qmd"