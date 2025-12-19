import argparse
import asyncio
from pathlib import Path

import pandas as pd
from selenium_driverless import webdriver
from selenium_driverless.types.by import By

BASE_DIR = Path("Odds-Scraping/BBL/Bet365")
HTML_DIR = BASE_DIR / "HTML"
MARKET_CONTAINER_XPATH = "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]"
MARKET_BUTTON_XPATH = "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]"
TEAM_NAMES_XPATH = "//div[contains(@class, 'rcl-ParticipantFixtureDetails_TeamNames')]"
MARKET_GROUP_XPATH = "//div[contains(@class, 'src-MarketGroup')]"
BET365_ROOT_URL = "https://www.bet365.com.au/#/AC/B3/C21107219/D720/E1246/F720/"

HTML_DIR.mkdir(parents=True, exist_ok=True)


async def click_optional_market_button(driver, label: str) -> None:
  """Attempt to expand a collapsible market group by button text."""
  try:
    button_xpath = (
      f"//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ') and text()='{label}']"
    )
    button = await driver.find_element(By.XPATH, button_xpath)
    await driver.execute_script("arguments[0].scrollIntoView(true);", button)
    await driver.execute_script("window.scrollBy(0, -150)")
    await button.click()
    print(f"Clicked {label}")
    await driver.sleep(1)
  except Exception:
    print(f"No {label} button was found")


async def scrape_lobby_and_urls() -> None:
  """Replicate the original URL harvesting logic from 01-get_bet365_html.py."""
  options = webdriver.ChromeOptions()

  async with webdriver.Chrome(options=options) as driver:
    await driver.get(BET365_ROOT_URL)
    await driver.sleep(0.1)

    elem = await driver.find_element(By.XPATH, MARKET_GROUP_XPATH, timeout=100)
    body_html = await elem.get_attribute("outerHTML")
    (HTML_DIR / "h2h_html.txt").write_text(body_html)

    team_elements = await driver.find_elements(By.XPATH, TEAM_NAMES_XPATH)
    for team_element in team_elements:
      print(await team_element.get_attribute("innerText"))

    match_url_list = []
    player_url_list = []
    first_over_url_list = []
    first_innings_url_list = []
    team_url_list = []

    for index in range(len(team_elements)):
      team_elements = await driver.find_elements(By.XPATH, TEAM_NAMES_XPATH)

      await driver.execute_script("arguments[0].scrollIntoView(true);", team_elements[index])
      await driver.execute_script("window.scrollBy(0, -150)")
      await driver.sleep(0.1)
      await team_elements[index].click()

      cur_url = await driver.current_url
      modified_match_url = cur_url + "I1/"
      modified_player_url = cur_url + "I2/"
      modified_first_over_url = cur_url + "I6/"
      modified_first_innings_url = cur_url + "I3/"
      modified_team_url = cur_url + "I4/"

      match_url_list.append(modified_match_url)
      player_url_list.append(modified_player_url)
      first_over_url_list.append(modified_first_over_url)
      first_innings_url_list.append(modified_first_innings_url)
      team_url_list.append(modified_team_url)

      print(modified_match_url)
      await driver.back()

    (BASE_DIR / "match_urls.csv").write_text("\n".join(match_url_list))
    (BASE_DIR / "player_urls.csv").write_text("\n".join(player_url_list))
    (BASE_DIR / "first_over_urls.csv").write_text("\n".join(first_over_url_list))
    (BASE_DIR / "first_innings_urls.csv").write_text("\n".join(first_innings_url_list))
    (BASE_DIR / "team_urls.csv").write_text("\n".join(team_url_list))


def read_url_series(csv_name: str) -> pd.Series:
  """Load URL CSV (written by scrape_lobby_and_urls)."""
  csv_path = BASE_DIR / csv_name
  return pd.read_csv(csv_path, header=None)[0]


async def capture_market_pages(
  urls: pd.Series,
  html_prefix: str,
  pre_actions=None,
) -> None:
  """Generalised version of the individual capture scripts."""
  options = webdriver.ChromeOptions()

  async with webdriver.Chrome(options=options) as driver:
    for index, url in enumerate(urls, start=1):
      try:
        await driver.get(url)
        await driver.find_element(By.XPATH, MARKET_BUTTON_XPATH, timeout=10)
        print(f"Getting URL {url} which is match {index}")

        if pre_actions:
          await pre_actions(driver)

        elem = await driver.find_element(By.XPATH, MARKET_CONTAINER_XPATH)
        body_html = await elem.get_attribute("outerHTML")
        (HTML_DIR / f"{html_prefix}_{index}.txt").write_text(body_html)
      except Exception as exc:
        print(f"An error occurred with URL {url}: {exc}. Moving to the next URL.")
        continue


async def scrape_players() -> None:
  """Replicates 02-get_bet365_player.py."""

  async def player_actions(driver):
    await click_optional_market_button(driver, "Player to Score Most 6s - Team")
    await click_optional_market_button(driver, "Race to 10 Runs")

  urls = read_url_series("player_urls.csv")
  await capture_market_pages(urls, "body_html_players_match", player_actions)


async def scrape_matches() -> None:
  """Replicates 03-get_bet365_match.py."""

  async def match_actions(driver):
    await click_optional_market_button(driver, "1st Wicket Method")
    await click_optional_market_button(driver, "Match - Runs in 1st x Overs - Team")
    await click_optional_market_button(driver, "Match Totals")

  urls = read_url_series("match_urls.csv")
  await capture_market_pages(urls, "body_html_match", match_actions)


async def scrape_teams() -> None:
  """Replicates 04-get_bet365_team.py."""
  urls = read_url_series("team_urls.csv")
  await capture_market_pages(urls, "body_html_team_match")


async def scrape_first_over() -> None:
  """Replicates 05-get_bet365_first_over.py."""
  urls = read_url_series("first_over_urls.csv")
  await capture_market_pages(urls, "body_html_first_over_match")


async def scrape_first_innings() -> None:
  """Replicates 06-get_bet365_first_innings.py."""
  urls = read_url_series("first_innings_urls.csv")
  await capture_market_pages(urls, "body_html_first_innings_match")


TASK_MAP = {
  "lobby": scrape_lobby_and_urls,
  "players": scrape_players,
  "matches": scrape_matches,
  "teams": scrape_teams,
  "first_over": scrape_first_over,
  "first_innings": scrape_first_innings,
}


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Scrape Bet365 BBL data using selenium_driverless."
  )
  parser.add_argument(
    "--tasks",
    nargs="+",
    choices=list(TASK_MAP.keys()),
    default=list(TASK_MAP.keys()),
    help="Tasks to run (default: all tasks).",
  )
  return parser.parse_args()


async def main(selected_tasks):
  for task in selected_tasks:
    print(f"Running task: {task}")
    await TASK_MAP[task]()


if __name__ == "__main__":
  args = parse_args()
  asyncio.run(main(args.tasks))
