# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        await driver.get('https://www.bet365.com.au/#/AC/B3/C20994011/D720/E1246/F720/')
        await driver.sleep(0.1)
        
        # wait 100s for elem to exist
        elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'src-MarketGroup')]", timeout=100)
        body_html = await elem.get_attribute('outerHTML')
        
        # Write html to file - overwrite existing file
        with open(f"Odds-Scraping/Internationals/Bet365/HTML/h2h_html.txt", 'w') as f:
            f.write(body_html)
        
        # Get all occurences of src-ParticipantFixtureDetailsHigher_Team, we want to click on each one
        team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'rcl-ParticipantFixtureDetails_TeamNames')]")
        
        # Print team elements inner text
        for team_element in team_elements:
            print(await team_element.get_attribute('innerText'))
        
        # URL List
        match_url_list = []
        player_url_list = []
        first_over_url_list = []
        first_innings_url_list = []
        team_url_list = []

        for index in range(len(team_elements)):
            # Get the team elements again as the page has been refreshed
            team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'rcl-ParticipantFixtureDetails_TeamNames')]")
            
            # Scroll into view, in the middle of screen
            await driver.execute_script("arguments[0].scrollIntoView(true);", team_elements[index])
            await driver.execute_script("window.scrollBy(0, -150)")
            
            # Wait 5 seconds
            await driver.sleep(0.1)
            
            # Click on the current element
            await team_elements[index].click()
            
            # Get Current URL
            cur_url = await driver.current_url

            # Append 'I1/' to URL
            modified_match_url = cur_url + 'I1/'
            
            # Append 'I2/' to URL
            modified_player_url = cur_url + 'I2/'
            
            # Append 'I6/' to URL
            modified_first_over_url = cur_url + 'I6/'
            
            ## Append 'I3/' to URL
            modified_first_innings_url = cur_url + 'I3/'

             ## Append 'I4/' to URL
            modified_team_url = cur_url + 'I4/'

            match_url_list.append(modified_match_url)
            player_url_list.append(modified_player_url)
            first_over_url_list.append(modified_first_over_url)
            first_innings_url_list.append(modified_first_innings_url)
            team_url_list.append(modified_team_url)

            print(modified_match_url)
            
            # Go back to the previous page
            await driver.back()
            
        # Write URL as a csv
        match_url_list = '\n'.join(match_url_list)
        with open(f"Odds-Scraping/Internationals/Bet365/match_urls.csv", 'w') as f:
           f.write(match_url_list)
           
        player_url_list = '\n'.join(player_url_list)
        with open(f"Odds-Scraping/Internationals/Bet365/player_urls.csv", 'w') as f:
           f.write(player_url_list)
           
        first_over_url_list = '\n'.join(first_over_url_list)
        with open(f"Odds-Scraping/Internationals/Bet365/first_over_urls.csv", 'w') as f:
           f.write(first_over_url_list)
           
        first_innings_url_list = '\n'.join(first_innings_url_list)
        with open(f"Odds-Scraping/Internationals/Bet365/first_innings_urls.csv", 'w') as f:
           f.write(first_innings_url_list)
           
        team_url_list = '\n'.join(team_url_list)
        with open(f"Odds-Scraping/Internationals/Bet365/team_urls.csv", 'w') as f:
           f.write(team_url_list)

asyncio.run(main())
