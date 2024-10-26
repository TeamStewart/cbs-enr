from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options
import os
import time
from datetime import datetime

def setup_driver(state):
    # Set up Chrome options
    chrome_options = Options()
    
    download_directory = os.path.abspath("data/raw/{}".format(state))
    
    # Specify the download directory and enable automatic downloads
    prefs = {"download.default_directory": download_directory,
             "download.prompt_for_download": False,
             "download.directory_upgrade": True,
             "safebrowsing.enabled": True}
    chrome_options.add_experimental_option("prefs", prefs)

    # Initialize the Selenium webdriver with Chrome options
    # Note: Update the path to chromedriver if necessary or use the Service class as per Selenium's latest practices
    service = Service(executable_path="path_to_chromedriver")  # Update this path
    driver = webdriver.Chrome(options = chrome_options)
    
    return driver, download_directory

def rename_file_with_timestamp(directory):
    # Wait for the new file to be downloaded completely
    time.sleep(10)  # Wait to ensure file has been fully downloaded
    files = os.listdir(directory)
    full_paths = [os.path.join(directory, f) for f in files if f.endswith(".csv")]

    if not full_paths:
        return None  # No csv files found

    # Find the most recent file based on last modification time
    most_recent_file = max(full_paths, key=os.path.getmtime)
    
    # Generate a timestamp for renaming
    timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
    new_filename = f"{os.path.splitext(os.path.basename(most_recent_file))[0]}_{timestamp}.csv"
    new_path = os.path.join(directory, new_filename)

    # Rename the file
    os.rename(most_recent_file, new_path)

    return new_filename

def get_file(path, county, state):
    # Initialize the driver
    driver, download_directory = setup_driver(state)
    # Navigate to the URL
    driver.get(path)
    
    # Wait for some time to ensure the file gets downloaded. Adjust the sleep time as necessary.
    time.sleep(8)
    
    # In Philadelphia County, PA, dynamically click download link
    if state == "PA" and county == "Philadelphia":
        download_button = driver.find_element(By.ID, "MainContent_LinkButton1")
        download_button.click()
        
        # wait more time for crdownload to unpack
        time.sleep(10)
        
        renamed_file = rename_file_with_timestamp(download_directory)
        
    # Close the webdriver
    driver.quit()

