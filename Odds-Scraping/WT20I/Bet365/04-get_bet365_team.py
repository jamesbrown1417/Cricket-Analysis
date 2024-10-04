# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
import pandas as pd
# Read csv (no header col)
url_df = pd.read_csv('Odds-Scraping/WT20I/Bet365/team_urls.csv', header=None)

# Convert first column to a list
url_df = url_df[0]

# Get HTML====================================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")
    
    async with webdriver.Chrome(options=options) as driver:
        for index, url in enumerate(url_df, start=1): # Start counting from 1 for match_n
            try:
                await driver.get(url)
                
                # Wait for cm-MarketGroupWithIconsButton_Text to exist
                await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]", timeout=100)
                
                # Print URL
                print(f"Getting URL {url} which is match {index}")

                # Get all elements with class 'msl-ShowMore_Link ' that has text 'Show more'
                # button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]")
                
                # print(len(button_elements))
                    
                # Scroll into view of each button, click it and wait 1 second
                # for button_element in button_elements:
                #    await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                #    await driver.execute_script("window.scrollBy(0, -150)")
                #    await button_element.click()
                #    await driver.sleep(1)
                    
                # Write out html to file------------------------------------------------
                # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_team = await elem.get_attribute('outerHTML')
                with open(f"Odds-Scraping/WT20I/Bet365/HTML/body_html_team_match_{index}.txt", 'w') as f:
                    f.write(body_html_team)
                        
            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())                    
