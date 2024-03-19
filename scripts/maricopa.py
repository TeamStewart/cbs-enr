from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
import os
import time

def get_maricopa(path):
    # Set up Chrome options
    chrome_options = Options()
    
    # Specify the download directory and enable automatic downloads
    prefs = {"download.default_directory": os.path.abspath("data/raw/AZ"),
             "download.prompt_for_download": False,
             "download.directory_upgrade": True,
             "safebrowsing.enabled": True}
    chrome_options.add_experimental_option("prefs", prefs)

    # Initialize the Selenium webdriver with Chrome options
    # Note: Update the path to chromedriver if necessary or use the Service class as per Selenium's latest practices
    service = Service(executable_path="path_to_chromedriver")  # Update this path
    driver = webdriver.Chrome(options = chrome_options)

    # Navigate to the URL
    driver.get(path)
    
    # Wait for some time to ensure the file gets downloaded. Adjust the sleep time as necessary.
    time.sleep(5)
    
    # Close the webdriver
    driver.quit()

