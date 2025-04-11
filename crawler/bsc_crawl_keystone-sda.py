# Installed packages: requests, beautifulsoup4, seleniumbase
# © 2025 https://github.com/y-neck
#!! This script is designed for scientific use only and should not be used for other purposes.

# import packages
from seleniumbase import Driver
from bs4 import BeautifulSoup
import json 
import time
import os
import copy

# import article_obj
from dictionary_entry import Article_obj

base_url = 'https://keystone-sda.ch/factchecking'
# initialize Seleniumbase Driver
driver = Driver(uc=True) # uc true: used to make the Chrome browser instance launched by SeleniumBase less detectable as an automated browser.

# load data if existing
all_data = []
if os.path.exists('keystone-sda.json'):     # INFO: Change file name for each new page
    with open('keystone-sda.json', 'r') as f:
        # try catch load data
        try: all_data = json.load(f)
        except  json.JSONDecodeError:
            print('JSONDecodeError: {JSONDecodeError}')

# start crawling
try:
    # open page with reconnect timeout and wait for 3 additional seconds
    driver.uc_open_with_reconnect(base_url, reconnect_time=10)
    time.sleep(3)

    # get page source
    page_source = driver.get_page_source()
    # parse page source
    soup = BeautifulSoup(page_source, 'html.parser')

    # handle 'show more' button until it is no longer present
    while True:
        try:
            driver.click('button.news-list-button')
            time.sleep(2)
        except Exception as e:
            print("No more 'Show more' button or failed to click:", e)
            break
    # parse page source
    page_source = driver.get_page_source()
    soup = BeautifulSoup(page_source, 'html.parser')

    # find all articles
    articles = soup.find_all('a', class_='news-link')   # INFO: Change selector to match article selector (e.g. article class)
    # DEBUG:
    print (f'Found {len(articles)} articles')

    base_id = 1000

    

    # iterate through all articles, incrementing the id
    for index, article in enumerate(articles, start=1):
        current_article = Article_obj()     # create a new instance for each article

        # assign dictionary values
        current_article.id = (base_id + index) * 10     # increment article ID
        article_url = article['href']      # retrieve article URL and assign it to the dictionary
        
        # DEBUG:
        if article_url:
            print(f'Found article {article_url}')
        else: 
            print('No article found!')
            break   

        current_article.article_url = article_url
        current_article.date = article.find('div', class_='news-date').text
        current_article.article_title = article.find('div', class_='news-headline').get_text(strip=True)  # remove leading and trailing whitespaces
        current_article.factchecking_platform = 1

        # Open article page
        driver.uc_open_with_reconnect(article_url, reconnect_time=10)
        time.sleep(2)
        # Re-parse the new article page
        article_source = driver.get_page_source()
        article_soup = BeautifulSoup(article_source, 'html.parser')

        # extract artefact url
        artefact_url = artefact_link = driver.find_element("css selector", "div.news-detail-repeatable-content a").get_attribute("href")
        current_article.artefact_url = artefact_url

        # extract original platform with case switch
        match True:
            case _ if "lnkd.in" in artefact_url or "linkedin.com" in artefact_url:
                current_article.original_platform = 11
            case _ if "instagram" in artefact_url or "insta" in artefact_url:
                current_article.original_platform = 12
            case _ if "twitter.com" in artefact_url or "x.com" in artefact_url:
                current_article.original_platform = 13
            case _ if "snap" in artefact_url:
                current_article.original_platform = 14
            case _ if "pin" in artefact_url:
                current_article.original_platform = 15
            case _ if "facebook.com" in artefact_url:
                current_article.original_platform = 16
            case _ if "tiktok.com" in artefact_url:
                current_article.original_platform = 17
            case _ if "archiv" in artefact_url or "perma" in artefact_url:
                current_article.original_platform = 30
            case _ if not artefact_url:
                current_article.original_platform = 90
            case _:
                current_article.original_platform = 20  # fallback for unknowns

        
        # DEBUG: print the current_article's dictionary
        #print(current_article.to_dict())

        # Append only if not already in the list (optional check)
        if current_article.__dict__ not in all_data:
            all_data.append(current_article.__dict__)       # check against existing entries before appending

    # DEBUG:
    print(f'Total articles stored: {len(all_data)}')

except Exception as e:
    print(f'Error retrieving data: {e}')

with open('keystone-sda.json', 'w') as f:       # INFO: Change depending on factchecking platform
    json.dump(all_data, f, indent=2, ensure_ascii=False)    # format with 2-space indentation, allow non-ascii

# Quit the driver to close the browser
    driver.close()
    driver.quit()
