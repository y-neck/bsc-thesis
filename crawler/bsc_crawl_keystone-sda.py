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

    # initialize article ID

    # find all articles
    articles = soup.find_all('a', class_='news-link')   # INFO: Change selector to match article selector (e.g. article class)
    # DEBUG:
    print (f'Found {len(articles)} articles')

    base_id = 1000

    # iterate through all articles, incrementing the id
    for index, article in enumerate(articles, start=1):
        current_article = Article_obj()     # create a new instance for each article

        # assign dictionary values
        current_article.id = base_id + index     # increment article ID
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

        # DEBUG: print the current_article's dictionary
        #print(current_article.to_dict())

        # Append only if not already in the list (optional check)
        if current_article.__dict__ not in all_data:
            all_data.append(current_article.__dict__)       # check against existing entries before appending

    # DEBUG:
    print(f'Total articles stored: {len(all_data)}')

except Exception as e:
    print(f'Error retrieving data: {e}')

with open('keystone-sda.json', 'w') as f:
    json.dump(all_data, f, indent=2, ensure_ascii=False)    # format with 2-space indentation, allow non-ascii





# # TODO: Open article page
#     driver.uc_open_with_reconnect(article_url, reconnect_time=10)
#     time.sleep(2)

#     article_source = driver.get_page_source()
#     article_soup = BeautifulSoup(article_source, 'html.parser')

#     # Fill in fields as available
#     title_tag = article_soup.find('h1')
#     if title_tag:
#         article_obj['title'] = title_tag.text.strip()

#     date_tag = article_soup.find('time')
#     if date_tag:
#         article_obj['date'] = date_tag.text.strip()

#     body_tag = article_soup.find('div', class_='article-body')  # Use actual class
#     if body_tag:
#         article_obj['content'] = body_tag.get_text(separator='\n').strip()

#     author_tag = article_soup.find('span', class_='author-name')  # If exists
#     if author_tag:
#         article_obj['author'] = author_tag.text.strip()

#     tags_container = article_soup.find('ul', class_='tags')
#     if tags_container:
#         article_obj['tags'] = [li.text.strip() for li in tags_container.find_all('li')]


# TODO: If not all shonw, click "show more" -> function